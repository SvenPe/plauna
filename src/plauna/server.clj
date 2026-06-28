(ns plauna.server
  (:require [cheshire.core :refer [parse-string]]
            [clojure.core.async :as async]
            [clojure.data :as cd]
            [clojure.java.io :as io]
            [clojure.string :as st]
            [compojure.core :as comp]
            [compojure.route :as route]
            [nrepl.server :as nrepl]
            [plauna.analysis :as analysis]
            [plauna.application :as app]
            [plauna.auth :as auth]
            [plauna.client :as client]
            [plauna.client.oauth :as oauth]
            [plauna.diagnostics :as diagnostics]
            [plauna.core.email :as core-email]
            [plauna.database :as db]
            [plauna.db-config :as db-cfg]
            [plauna.db-migration :as db-mig]
            [plauna.files :as files]
            [plauna.markup :as markup]
            [plauna.messaging :as messaging]
            [plauna.preferences :as p]
            [plauna.settings :as settings]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [ring.util.codec :refer [base64-decode]]
            [ring.util.response :refer [response redirect]]
            [selmer.parser :as selmer]
            [taoensso.telemere :as t])
  (:import [java.net ServerSocket]
           [java.util UUID]
           [java.util.concurrent ExecutorService Executors]
           [org.eclipse.jetty.server Server]
           [org.eclipse.jetty.util.thread QueuedThreadPool]))

(set! *warn-on-reflection* true)

;; Holds {:jetty ^Server, :executor ^ExecutorService} when running, nil otherwise.
(defonce server (atom nil))

(defonce repl-server (atom nil))

(def html-headers {"Content-Type" "text/html; charset=UTF-8"})

(defonce global-messages (atom []))

(defn add-to-messages [message] (swap! global-messages (fn [messages] (conj messages message))))

(defn interleave-all [& seqs]
  (reduce (fn [acc index] (into acc (map #(get % index) seqs)))
          []
          (range (apply max (map count seqs)))))

(defn vectorize [items]
  (if (vector? items) items [items]))

(defn flatten-map [param-map]
  (let [message-ids (vectorize (get param-map :message-id []))
        languages (vectorize (get param-map :language []))
        categories (vectorize (get param-map :category []))
        language-confidence (vectorize (get param-map :language-confidence []))
        category-confidence (vectorize (get param-map :category-confidence))]
    (map (fn [vect] {:message-id (nth vect 0) :language (nth vect 1) :category (nth vect 2) :language-confidence (nth vect 3) :category-confidence (nth vect 4)}) (partition 5 (interleave-all message-ids languages categories language-confidence category-confidence)))))

(defn params->update-request [params]
  (let [language (:language params)
        category-id (:category params)
        language-exists (and (some? language) (seq language))
        category-exists (and (some? category-id) (seq category-id))]
    {:language   (when language-exists (:language params))
     :category-id (when category-exists (Integer/parseInt (:category params)))
     :category-confidence  (when category-exists (Float/parseFloat (:category-confidence params)))
     :language-confidence (when language-exists (Float/parseFloat (:language-confidence params)))}))

(defn save-metadata-form [params]
  (let [transformed (flatten-map params)]
    (dorun (map (fn [x] (let [request (params->update-request x)]
                          (db/update-metadata (:message-id x) (:category-id request) (:category-confidence request) (:language request) (:language-confidence request)))) transformed))))

(defn success-html-with-body [body]
  {:status  200
   :headers {"Content-Type" "text/html; charset=UTF-8"}
   :body    body})

(defn safe-redirect-path
  "Only allow same-origin, relative redirect targets (a single leading slash, not \"//host\" or a scheme).
   Anything else (an absolute URL, protocol-relative URL, or nil) falls back to `default`, preventing an
   attacker-supplied redirect-url from turning into an open redirect."
  [target default]
  (if (and (string? target)
           (re-matches #"/[^/].*|/" target))
    target
    default))

(defn same-origin-referer
  "Extract the path (+query) from the request's Referer only when its host matches the request Host,
   so a cross-origin Referer can never be used as an open-redirect target. Returns nil otherwise."
  [request]
  (let [referer (get (:headers request) "referer")
        host    (get (:headers request) "host")]
    (when (and referer host)
      (try
        (let [uri (java.net.URI. referer)]
          (when (= (.getAuthority uri) host)
            (let [path (.getRawPath uri)
                  q    (.getRawQuery uri)]
              (str (if (st/blank? path) "/" path) (when q (str "?" q))))))
        (catch Exception _ nil)))))

(defn redirect-to-referer [request]
  {:status 303
   ;; Fall back to the app root when the Referer is absent or off-site, so we never emit a 303 with a
   ;; nil Location or redirect the user to an attacker-controlled origin.
   :headers {"Location" (or (same-origin-referer request) "/")}})

(defn redirect-request
  ([request]
   {:status 303 :headers {"Location" (safe-redirect-path (get-in request [:params :redirect-url]) (:uri request))}})
  ([request messages]
   (swap! global-messages (fn [m] (conj m messages)))
   {:status 303 :headers {"Location" (safe-redirect-path (get-in request [:params :redirect-url]) (:uri request))}}))

(defn- normalize-prefs [prefs]
  (mapv #(update % :use_in_training (fn [v] (or (true? v) (= 1 v)))) prefs))

(defn language-preferences []
  (let [preferences (db/get-language-preferences)
        languages (filterv #(not (= "n/a" %)) (mapv :language (db/get-languages)))]
    (if (empty? languages)
      []
      (normalize-prefs
       (if (< (count preferences) (count languages))
         (let [existing-languages-in-pref (mapv :language preferences)
               diff (cd/diff (set existing-languages-in-pref) (set languages))]
           (db/add-language-preferences
            (mapv vector (second diff) (repeat (count (second diff)) false)))
           (db/get-language-preferences))
         preferences)))))

(defn languages-to-use-in-training []
  (map :language (db/get-activated-language-preferences)))

(defn write-all-categorized-emails-to-training-files []
  (files/delete-files-with-type :train)
  (doseq [language (languages-to-use-in-training)
          :let [entity-query {:entity :enriched-email :page {:size 100 :page 1}}
                sql-query {:where [:and [:<> :category nil] [:= :language language]]}
                write-func (fn [data]
                             (let [formatted (analysis/format-training-data data)]
                               (when (seq formatted)
                                 (files/write-to-training-file language formatted))))]]
    (core-email/iterate-over-all-pages db/fetch-data write-func entity-query sql-query false)))

(defn write-emails-to-training-files-and-train []
  (if (seq (languages-to-use-in-training))
    (do (write-all-categorized-emails-to-training-files)
        (doseq [training-model (remove nil? (analysis/train-data (files/training-files)))]
          (let [os (io/output-stream (files/model-file (:language training-model)))]
            (analysis/serialize-and-write-model! (:model training-model) os))))
    {:type :alert :content "There are no selected languages to train in. Cannot proceed."}))

(defn categorize-content [content language] ;; FIXME This kills the process if content is nil
  (let [category (analysis/categorize content (files/model-file language))]
    {:id         (:id (db/category-by-name (:name category)))
     :name       (:name category)
     :confidence (:confidence category)}))

(defn categorize-uncategorized-n-emails [n]
  (let [languages-to-use (map :language (db/get-activated-language-preferences))
        uncategorized-bodies (:data (db/fetch-data {:entity :body-part :page {:page 0 :size n}} {:where [:and [:in :language languages-to-use] [:<> :language nil] [:= :category nil] [:= :mime-type "text/html"]]}))
        trained-emails (map (fn [email] (conj {:message-id (-> email :body-part :message-id)} (categorize-content (-> email :body-part :sanitized-content) (-> email :metadata :language)))) uncategorized-bodies)]
    (doseq [trained-email trained-emails] (db/update-metadata-category (:message-id trained-email) (:id trained-email) (:confidence trained-email)))))

(defn mime-type-statistics [period]
  ;; MariaDB does not allow SELECT aliases in WHERE; reference the source column directly.
  (let [iv (db/interval-for-honey period)]
    (db/query-db {:select [[[:count :headers.message-id] :count] :bodies.mime-type [iv :interval]] :from [:bodies]
                  :join [:headers [:= :bodies.message-id :headers.message_id]]
                  :where [:is-not :headers.date nil]
                  :group-by [:interval :bodies.mime-type]
                  :order-by [[:count :desc]]})))

(defn language-statistics-by-period [period]
  (db/query-db {:select [[[:count :metadata.language] :count] :metadata.language [(db/interval-for-honey period) :interval]] :from [:metadata]
                :join [:headers [:= :metadata.message-id :headers.message_id]]
                :group-by [:language :interval]}))

(defn category-statistics-by-period [period]
  ;; MariaDB does not allow SELECT aliases in WHERE; inline the interval expression there.
  (let [year       (:year period)
        iv         (db/interval-for-honey (:interval period))
        categories (reduce (fn [acc el] (merge acc {(:id el) (:name el)})) {} (db/get-categories))
        statistics (if (some? year)
                     (db/query-db {:select [[[:count :metadata.category] :count] :metadata.category [iv :interval]] :from [:metadata]
                                   :join [:headers [:= :metadata.message-id :headers.message_id]]
                                   :where [:and [:<> :category nil] [:like iv (str year "%")]]
                                   :group-by [:category :interval]})
                     (db/query-db {:select [[[:count :metadata.category] :count] :metadata.category [iv :interval]] :from [:metadata]
                                   :join [:headers [:= :metadata.message-id :headers.message_id]]
                                   :where [:<> :category nil]
                                   :group-by [:category :interval]}))]
    (map (comp
          (fn [map] (if (= 0 (get map :category)) map (update map :category (fn [cat-key] (get categories cat-key)))))
          (fn [map] (update map :category #(if (int? %) % (Integer/parseInt %))))) statistics)))

(defn enriched-email-by-message-id [id] (first (db/fetch-data {:entity :enriched-email :strict false} {:where [:= :message-id id]})))

(defn refetch-email-and-fill!
  "Re-read a message from the IMAP server and fill in data that is now parseable: the body parts (saved
   if missing; existing rows are left untouched) and the language when it was never detected. Returns a
   message map for the UI."
  [message-id]
  (if-let [refetched (client/refetch-message-by-id message-id)]
    (do
      ;; Only save parts that aren't already stored. Attachment rows have a nil :content, and a UNIQUE
      ;; constraint does not dedupe NULLs, so blindly re-saving them would accumulate a duplicate
      ;; attachment row on every refetch. Keying on mime-type + filename keeps refetch idempotent for
      ;; both text and attachment parts.
      (let [stored-key      (juxt :mime-type :filename)
            already-stored  (set (map stored-key (db/fetch-bodies-for [message-id])))
            missing-parts   (remove (comp already-stored stored-key) (:body refetched))]
        (when (seq missing-parts) (db/save-bodies missing-parts)))
      ;; The body text is available now, so fill in the language if it was never detected. We leave an
      ;; already-set language alone so a manual correction is never clobbered.
      (let [current-lang (:language (db/fetch-metadata message-id))]
        (when (or (nil? current-lang) (st/blank? (str current-lang)) (= "n/a" current-lang))
          (let [lang (analysis/language-result refetched)]
            (when (and (:code lang) (not= "n/a" (:code lang)))
              (db/update-metadata-language message-id (:code lang) (:confidence lang))))))
      {:type :success :content "Re-fetched the email from the server and filled in its contents."})
    {:type :alert :content "Could not re-fetch this email: it was not found on any connected account."}))

;; TODO change name template
(def emails-template {:size {:default 20 :type-fn Integer/parseInt}
                      :page {:default 1 :type-fn Integer/parseInt}
                      :filter {:default "all" :type-fn identity}
                      :search-field {:default "subject" :type-fn identity}
                      :search-text {:default nil :type-fn identity}
                      :date-from {:default nil :type-fn identity}
                      :date-to {:default nil :type-fn identity}})

(defn template->request-parameters [template]
  (fn [rp] (reduce (fn [acc [k v]] (if (contains? rp k)
                                     (conj acc {k ((:type-fn v) (get rp k))})
                                     (conj acc {k (:default v)})))
                   {} template)))

(defn add-sanitized-text-to-enriched-email [email]
  {:header (:header email)
   :metadata (:metadata email)
   :participants (:participants email)
   :body (map (fn [body-part] (if (core-email/body-text-content? body-part)
                                (conj body-part {:sanitized-content (analysis/normalize-body-part body-part)})
                                body-part)) (:body email))})

(defn get-status-repl-server [] {:status (some? @repl-server) :port 7888})

(defn connection-information [id] (let [conn (db/get-connection id)] (merge conn (client/monitor->map (get @client/connections (:id conn))))))
(defn connection-folders [conn]
  (if (= true (:connected conn))
    (client/folders-in-store (:store (client/connection-data-from-id (:id conn))))
    []))

(defn empty-global-messages [] (reset! global-messages []))

(defmacro result-with-messages [markup-call messages-var]
  `(if (seq @~messages-var)
     (let [messages# @~messages-var]
       (reset! ~messages-var [])
       (~@markup-call messages#))
     ~markup-call))

(defn make-routes [context]
  (comp/routes

   (route/resources "/")

   (comp/GET "/login" {} (success-html-with-body (markup/login-page)))

   (comp/POST "/login" request
     (if (auth/verify-web-password? (-> request :params :password))
       (-> (redirect "/")
           (assoc :session (assoc (:session request) :authenticated true)))
       (success-html-with-body (markup/login-page {:error "Invalid password."}))))

   (comp/GET "/logout" {}
     (-> (redirect "/login") (assoc :session nil)))

   (comp/GET "/admin/threads" {}
     ;; A full thread dump for diagnosing freezes. Also written to the log. Returned as plain text so
     ;; it can be copied directly from the browser.
     (let [dump (diagnostics/thread-dump-string)]
       (diagnostics/log-thread-dump! "requested via /admin/threads")
       {:status  200
        :headers {"Content-Type" "text/plain; charset=UTF-8"}
        :body    dump}))

   (comp/GET "/admin/database" {}
     (let [cfg        (db-cfg/load-config)
           saved      (when (= :mariadb (:type cfg)) (dissoc cfg :password))
           db-status  (if (= :mariadb (db/db-type))
                        {:type "mariadb" :host (:host cfg) :port (:port cfg)
                         :name (:name cfg) :user (:user cfg)}
                        {:type "sqlite" :path (files/path-to-db-file)})]
       (success-html-with-body
        (selmer/render-file "admin-database.html"
                            {:db-status db-status
                             :saved-config saved
                             :sqlite-exists (.exists (clojure.java.io/file (files/path-to-db-file)))
                             :header "Database"
                             :active-nav :admin}))))

   (comp/POST "/admin/database/config" request
     (let [{:keys [host port name user password]} (:params request)
           existing (db-cfg/load-config)
           cfg {:type     :mariadb
                :host     host
                :port     (Integer/parseInt port)
                :name     name
                :user     user
                :password (if (st/blank? password) (:password existing "") password)}]
       (db-cfg/save-config! cfg)
       (redirect "/admin/database?saved=1")))

   (comp/POST "/admin/database/test" request
     (let [{:keys [host port name user password]} (:params request)
           existing (db-cfg/load-config)
           cfg {:host     host
                :port     (Integer/parseInt port)
                :name     name
                :user     user
                :password (if (st/blank? password) (:password existing "") password)}
           result (db-mig/test-connection! cfg)]
       (success-html-with-body
        (selmer/render-file "admin-database.html"
                            {:db-status (if (= :mariadb (db/db-type))
                                          {:type "mariadb" :host (:host existing) :port (:port existing)
                                           :name (:name existing) :user (:user existing)}
                                          {:type "sqlite" :path (files/path-to-db-file)})
                             :saved-config (dissoc (db-cfg/load-config) :password)
                             :sqlite-exists (.exists (clojure.java.io/file (files/path-to-db-file)))
                             :message (if (:ok result)
                                        {:type "success" :text "Connection successful!"}
                                        {:type "error"   :text (str "Connection failed: " (:error result))})
                             :header "Database"
                             :active-nav :admin}))))

   (comp/POST "/admin/database/migrate" {}
     (let [result (db-mig/migrate!)]
       (success-html-with-body
        (selmer/render-file "admin-database.html"
                            {:db-status (if (= :mariadb (db/db-type))
                                          (let [cfg (db-cfg/load-config)]
                                            {:type "mariadb" :host (:host cfg) :port (:port cfg)
                                             :name (:name cfg) :user (:user cfg)})
                                          {:type "sqlite" :path (files/path-to-db-file)})
                             :saved-config (dissoc (db-cfg/load-config) :password)
                             :sqlite-exists (.exists (clojure.java.io/file (files/path-to-db-file)))
                             :message (cond
                                        (not (:ok result))
                                        {:type "error" :text (str "Migration failed: " (:error result))}
                                        (pos? (:skipped-total result 0))
                                        {:type "error" :text (str "Migration finished with " (:skipped-total result) " skipped row(s) — check the logs for details. Do not restart until the issue is resolved.")}
                                        :else
                                        {:type "success" :text "Migration complete with no losses. Save the configuration above and restart Plauna to switch to MariaDB."})
                             :migration-counts (when (:ok result)
                                                 (map (fn [[t v]] {:table t :inserted (:inserted v) :skipped (:skipped v) :total (:total v)}) (:counts result)))
                             :header "Database"
                             :active-nav :admin}))))

   (comp/GET "/admin/password" {}
     (success-html-with-body (markup/password-page {:env-managed (auth/password-from-env-var?)})))

   (comp/POST "/admin/password" request
     (let [{:keys [current-password new-password confirm-password]} (:params request)]
       (cond
         (not (auth/verify-web-password? current-password))
         (redirect-request request {:type :alert :content "Current password is incorrect."})

         (not= new-password confirm-password)
         (redirect-request request {:type :alert :content "New password and confirmation do not match."})

         (< (count (or new-password "")) 8)
         (redirect-request request {:type :alert :content "New password must be at least 8 characters long."})

         :else
         (do (auth/set-password! new-password)
             (redirect-request request {:type :success :content "Password changed successfully."})))))

   (comp/GET "/" {} (let [data (db/yearly-email-stats)]
                      (if (> (count data) 0)
                        {:status  302
                         :headers {"Location" "/emails"}}
                        {:status  302
                         :headers {"Location" "/admin"}})))

   (comp/GET "/admin" {}
     (if (seq @global-messages)
       (let [messages @global-messages]
         (swap! global-messages (fn [_] []))
         (success-html-with-body (markup/administration messages)))
       (success-html-with-body (markup/administration {:repl (get-status-repl-server)}))))

   (comp/POST "/emails/parse" request
     (let [temp-file (get-in request [:params :filename :tempfile])]
       (files/read-emails-from-mbox (io/input-stream temp-file) @messaging/main-chan)
       (redirect-request request {:type :success :content (str "Starting to parse file: " temp-file)})))

   (comp/GET "/admin/categories" {}
     (let [categories (db/get-categories)]
       (success-html-with-body (markup/categories-page categories))))

   (comp/GET "/admin/languages" {}
     (success-html-with-body
      (markup/languages-admin-page (language-preferences))))

   (comp/GET "/admin/preferences" {}
     (let [language-datection-threshold (p/language-detection-threshold)
           categorization-threshold (p/categorization-threshold)
           client-health-check-interval (p/client-health-check-interval)
           log-level (p/log-level)]
       (success-html-with-body (markup/preferences-page
                                {:language-detection-threshold language-datection-threshold
                                 :categorization-threshold categorization-threshold
                                 :log-level log-level
                                 :client-health-check-interval client-health-check-interval}))))

   (comp/POST "/admin/preferences" request
     (doseq [param (dissoc (:params request) :redirect-url)]
       (p/update-preference (first param) (second param)))
     (t/set-min-level! (p/log-level))
     (redirect-request request))

   (comp/POST "/admin/languages" {params :params}
     (let [langs-to-use (if (vector? (:use params)) (:use params) [(:use params)])]
       (doseq [preference (mapv (fn [id language]
                                  {:id id :language language :use (some? (some #(= language %) langs-to-use))})
                                (vectorize (:id params))
                                (vectorize (:language params)))]
         (db/update-language-preference preference)))
     (let [language-preferences (language-preferences)]
       (success-html-with-body (markup/languages-admin-page language-preferences))))

   (comp/POST "/admin/categories" {params :params}
     (app/create-new-category! context (:name params) (:destination-folder params))
     {:status  301
      :headers {"Location" "/admin/categories"}
      :body    (markup/administration {:repl (get-status-repl-server)})})

   (comp/POST "/admin/categories/:id" {route-params :route-params params :params}
     (app/update-category-destination-folder! context (:id route-params) (:name params) (:destination-folder params))
     {:status  301
      :headers {"Location" "/admin/categories"}
      :body    (markup/administration {:repl (get-status-repl-server)})})

   (comp/DELETE "/admin/categories/:id" {route-params :route-params}
     (db/delete-category-by-id (:id route-params))
     {:status  301
      :headers {"Location" "/admin/categories"}
      :body    (markup/administration {:repl (get-status-repl-server)})})

   (comp/POST "/admin/database" {}
     (files/check-and-create-database-file)
     (db/create-db)
     {:status  301
      :headers {"Location" "/admin"}
      :body    (markup/administration {:repl (get-status-repl-server)})})

   (comp/GET "/statistics" {}
     (success-html-with-body (markup/statistics-overall (db/yearly-email-stats) (mime-type-statistics :yearly) (language-statistics-by-period :yearly) (category-statistics-by-period {:interval :yearly}))))

   (comp/POST "/metadata/category" request
     ;; Recategorize a single email immediately (the e-mail list's category dropdown calls this on
     ;; change, so no "Batch Update" click is needed). The category is only persisted when the IMAP move
     ;; succeeds, so the stored category always reflects the real IMAP location. Clearing (n/a) always
     ;; saves without a move. Returns 204 on full success, 200 (amber in the UI) when the move failed,
     ;; and 500 on unexpected error.
     (let [message-id (:message-id (:params request))
           category (:category (:params request))
           new-category-id (when (seq category) (Integer/parseInt category))]
       (if (nil? new-category-id)
         ;; Clearing the category — no move, just save.
         (do (db/update-metadata-category message-id nil 1.0)
             {:status 204})
         (let [email-before (enriched-email-by-message-id message-id)
               new-category-name (get (first (filter #(= (:id %) new-category-id) (db/get-categories))) :name "")
               process (app/move-email-to-category email-before new-category-name context)
               moved? (= :ok (:result process))]
           (if moved?
             (do (db/update-metadata-category message-id new-category-id 1.0)
                 {:status 204})
             ;; Move failed — do not update the category so metadata stays consistent with the real folder.
             {:status 200 :headers {"Content-Type" "text/plain"} :body "saved-not-moved"})))))

   (comp/POST "/metadata/language" request
     ;; Update a single email's detected language immediately (the language field calls this on change).
     ;; A blank value is treated as nil (clearing the language) to match the old batch-update behaviour
     ;; and avoid leaving an empty string that confuses enriched-only filters.
     (let [{:keys [message-id language]} (:params request)
           lang (when (seq language) language)]
       (db/update-metadata-language message-id lang 1.0)
       {:status 204}))

   (comp/POST "/metadata" request
     (if (some? (:move (:params request)))
       (let [message-id (:message-id (:params request))
             email-before-update (enriched-email-by-message-id message-id)
             new-category-id (Integer/parseInt (:category (:params request)))
             new-category-name (get (first (filter #(= (:id %) new-category-id) (db/get-categories))) :name "")
             process (app/move-email-to-category email-before-update new-category-name context)]
         (if (= :error (:result process))
           (add-to-messages (:message process))
           (save-metadata-form (:params request))))
       (save-metadata-form (:params request)))
     (redirect-to-referer request))

   (comp/POST "/training" request
     (let [result (write-emails-to-training-files-and-train)]
       (when (some? result) (swap! global-messages (fn [mess] (conj mess result))))
       (redirect-to-referer request)))

   (comp/POST "/training/new" request
     (let [n (get (:route-params request) :new 20)]
       (categorize-uncategorized-n-emails n)
       (redirect-request request)))

   (comp/GET "/emails" {params :params}
     (let [parse-fn (template->request-parameters emails-template)
           result (app/fetch-emails context (parse-fn params))]
       (success-html-with-body (result-with-messages (markup/list-emails (:data result) (:parameters result) (:categories (:optional result))) global-messages))))

   (comp/GET "/emails/:id" [id]
     (let [decoded-id (new String ^"[B" (base64-decode id))
           email-data (add-sanitized-text-to-enriched-email (enriched-email-by-message-id decoded-id))
           categories (conj (db/get-categories) {:id nil :name "n/a"})]
       (success-html-with-body (result-with-messages (markup/list-email-contents email-data categories) global-messages))))

   (comp/DELETE "/emails/:id" [id]
     (db/delete-email-by-message-id (new String ^"[B" (base64-decode id)))
     {:status  200})

   (comp/POST "/emails/:id/refetch" [id :as request]
     (add-to-messages (refetch-email-and-fill! (new String ^"[B" (base64-decode id))))
     (redirect-to-referer request))

   (comp/GET "/admin/connections" _
     (let [messages @global-messages]
       (empty-global-messages)
       (if (seq messages)
         (response (markup/connections-list (mapv (fn [conn] (merge conn (client/monitor->map (get @client/connections (:id conn))))) (db/get-connections)) messages))
         (response (markup/connections-list (mapv (fn [conn] (merge conn (client/monitor->map (get @client/connections (:id conn))))) (db/get-connections)))))))

   (comp/POST "/admin/connections" request
     (let [params (:params request)
           config {:host (get params :host) :user (get params :user) :secret (get params :secret) :folder (get params :folder) :debug (= "true" (get params :debug)) :security (get params :security) :port (when (seq (get params :port)) (Integer/parseInt (get params :port))) :check-ssl-certs (= "true" (get params :check-ssl-certs))}
           id (client/id-from-config config)]
       (db/add-connection (merge config {:id id}))
       (redirect-request request)))

   (comp/DELETE "/admin/connections/:id" request
     (let [params (:params request)]
       (db/delete-connection (get params :id))
       {:status 200}))

   (comp/GET "/admin/new-connection" []
     (let [providers (db/get-auth-providers)]
       {:status 200
        :header html-headers
        :body   (markup/new-connection providers)}))

   (comp/DELETE "/admin/auth-providers/:id" request
     (let [params (:params request)
           body (parse-string (slurp (:body request)) true)]
       (db/delete-auth-provider (get params :id))
       (if (empty? (:conn-id body))
         (redirect "/admin/new-connection" 303)
         (redirect (str "/admin/connections/" (:conn-id body) 303)))))

   (comp/POST "/admin/auth-providers" request
     (let [params (:params request)]
       (db/add-auth-provider (dissoc params :redirect-url))
       (if (= "/admin/connections/" (:redirect-url params))
         (redirect-request (assoc-in request [:params :redirect-url] "/admin/new-connection"))
         (redirect-request request))))

   (comp/PUT "/admin/auth-providers/:id" request
     (let [params (:params request)]
       (db/update-auth-provider params)))

   (comp/GET "/admin/connections/:id" [id]
     (let [conn-info (connection-information id)
           providers (db/get-auth-providers)
           categories (db/get-categories)]
       (if (seq @global-messages)
         (let [messages @global-messages]
           (swap! global-messages (fn [_] []))
           (success-html-with-body (markup/connection (assoc conn-info :auth-providers providers) (connection-folders conn-info) messages categories)))
         (success-html-with-body (markup/connection (assoc conn-info :auth-providers providers) (connection-folders conn-info) categories)))))

   (comp/PUT "/admin/connections/:id" request
     (let [params (:params request)]
       (db/update-connection {:id (get params :id) :host (get params :host) :user (get params :user) :secret (get params :secret) :folder (get params :folder) :debug (= "true" (get params :debug)) :security (get params :security) :port (when (seq (get params :port)) (Integer/parseInt (get params :port))) :check-ssl-certs (= "true" (get params :check-ssl-certs)) :auth-type (get params :auth-type) :auth-provider (get params :auth-provider)})
       {:status 200}))

   (comp/POST "/admin/connections/:id/controls" request
     (let [id (:id (:route-params request))
           operation (:operation (:params request))]
       (cond (= "reconnect" operation) (do (client/reconnect (client/connection-data-from-id id)) (redirect-request request))
             (= "disconnect" operation) (do (client/disconnect (client/connection-data-from-id id)) (redirect-request request))
             (= "connect" operation)
             (let [action (app/connect-to-client context id)]
               (cond
                 (= :redirect (:result action))
                 (let [csrf (.toString (UUID/randomUUID))]
                   (-> (redirect (oauth/authorize-uri (:provider action) csrf))
                       (assoc :session (merge (:session request) {:oauth-csrf csrf :connection-id id :provider (:provider action)}))))
                 (= :ok (:result action))
                 (redirect-request request)
                 (= :error (:result action))
                 (redirect-request request {:type :alert :content "Connection failed. Please see the logs for the details."})))
             (= "parse" operation) (let [params (:params request)
                                         folder (:folder params)
                                         move (some? (:move params))
                                         assigned-category (when-not (st/blank? (:assigned-category params)) (db/category-by-id (:assigned-category params)))
                                         conn-data (client/connection-data-from-id id)
                                         message-count (app/read-emails-from-folder conn-data folder {:move? move :assigned-category (:name assigned-category) :assigned-category-id (:id assigned-category)} context)]
                                     (swap! global-messages (fn [mess] (conj mess {:type :success :content (str "Started parsing " folder " asynchronously. There are " message-count " emails in the folder. Move folders after parsing: " move)})))
                                     (redirect-request request)))))

   (comp/POST "/metadata/languages" request
     (let [limiter (messaging/channel-limiter :enriched-email)
           process-fn (fn [enriched-emails]
                        (doseq [enriched-email enriched-emails]
                          (async/>!! limiter :token)
                          (async/>!! @messaging/main-chan {:type :language-detection-request :options {} :payload enriched-email})))]
       (core-email/iterate-over-all-pages db/fetch-data process-fn {:entity :enriched-email :strict false :page {:page 1 :size 500}} {:where [:= :language nil]} true))
     (redirect-request request))

   (comp/POST "/repl" request
     (let [operation (get-in request [:params :operation])]
       (cond (= operation "start") (swap! repl-server (fn [_] (t/log! :info "Starting repl server") (nrepl/start-server :bind "127.0.0.1" :port 7888)))
             (= operation "stop") (swap! repl-server (fn [_] (t/log! :info "Stopping repl server") (nrepl/stop-server @repl-server) nil))
             :else (t/log! :error ["Unsupported operation" operation "at /repl"]))
       (redirect-request request)))

   (comp/GET "/oauth2/callback" request
     (let [params (:params request)
           session (:session request)
           state (:state params)
           expected-csrf (:oauth-csrf session)]
       (if (and (seq state) (seq expected-csrf) (= state expected-csrf))
         (try
           (let [response (oauth/exchange-code-for-access-token (:provider session) (:code params))]
             (db/save-oauth-token (assoc response :connection-id (:connection-id session)))
             (app/connect-to-client context (:connection-id session)))
           (redirect "/admin/connections")
           (catch Exception e (t/log! :error e) (redirect "/admin/connections")))
         (do (t/log! :warn "OAuth callback rejected: missing or mismatched CSRF token.")
             (redirect "/admin/connections")))))

   (route/resources "/")))

(defn upload-progress [_ bytes-read content-length item-count]
  (t/log! {:level :info
           :limit  [[1 5000]]
           :limit-by content-length
           :let [read-percent  (* 100 (float (/ bytes-read content-length)))]}
          ["Writing" item-count "files. Read" read-percent "% until now. Total length: " content-length]))

(defn- public-path?
  "Paths reachable without authentication: the login endpoint, the OAuth callback, and the static assets the login page needs."
  [^String uri]
  (or (= uri "/login")
      (= uri "/oauth2/callback")
      (.startsWith uri "/css/")
      (.startsWith uri "/favicon")
      (.startsWith uri "/android-chrome")
      (= uri "/plauna-banner.png")
      (= uri "/site.webmanifest")))

(defn wrap-authentication
  "Require a logged-in session for every request except public paths. Unauthenticated requests are
   redirected to the login page."
  [handler]
  (fn [request]
    (if (or (public-path? (:uri request))
            (get-in request [:session :authenticated]))
      (handler request)
      (redirect "/login"))))

(defn wrap-exception-handling
  "Catch exceptions escaping a handler so bad input or unexpected failures return a clean response
   instead of a 500 with a leaked stack trace. A NumberFormatException (e.g. a non-numeric port or
   category param) becomes a 400 rather than crashing the request handler."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch NumberFormatException e
        (t/log! {:level :warn :error e} ["Non-numeric value in a numeric parameter for" (:uri request)])
        {:status 400 :headers html-headers :body "Invalid request: a numeric field received a non-numeric value."})
      (catch Throwable e
        (t/log! {:level :error :error e} ["Unhandled error while processing" (:uri request)])
        {:status 500 :headers html-headers :body "An unexpected error occurred."}))))

(defn app [context] (-> (fn [req] ((make-routes context) req))
                        wrap-authentication
                        wrap-keyword-params
                        (wrap-multipart-params {:progress-fn upload-progress})
                        wrap-params
                        wrap-exception-handling
                        (wrap-session {:store (cookie-store {:key (settings/session-key)})
                                       ;; HttpOnly keeps the cookie out of JS; SameSite=Lax blocks forged
                                       ;; cross-site POSTs (CSRF) while still allowing the OAuth provider's
                                       ;; top-level redirect back to /oauth2/callback to carry the session.
                                       :cookie-attrs {:http-only true :same-site :lax}})))

(defn get-random-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn start-server [context]
  (let [config (:config context)
        port (if (some? (-> (:server config) :port)) (-> (:server config) :port) (get-random-port))
        new-app (app context)
        executor (Executors/newVirtualThreadPerTaskExecutor)]
    (t/log! :info [(str "Starting server: http://0.0.0.0:" port)])
    (try
      (let [jetty (jetty/run-jetty (fn [req] (new-app req))
                                   {:port        port
                                    :join?       false
                                    :configurator (fn [^Server s]
                                                    (.setVirtualThreadsExecutor
                                                     ^QueuedThreadPool (.getThreadPool s)
                                                     executor))})]
        (reset! server {:jetty jetty :executor executor}))
      (catch Exception e
        (.close ^ExecutorService executor)
        (throw e)))))

(defn stop-server []
  (if-some [{^Server jetty :jetty ^ExecutorService executor :executor} @server]
    (do
      (let [port (.getPort (.getURI jetty))]
        (t/log! {:level :info} ["Stopping server on port" port]))
      (try
        (.stop jetty)
        (finally
          (.close executor)))
      (reset! server nil)
      nil)
    (do (t/log! :info "No server running.")
        nil)))
