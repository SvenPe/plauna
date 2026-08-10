(ns plauna.server-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [honey.sql :as honey]
            [plauna.application :as app]
            [plauna.auth :as auth]
            [plauna.client :as client]
            [plauna.client.oauth :as oauth]
            [plauna.database :as db]
            [plauna.files :as files]
            [plauna.preferences :as preferences]
            [plauna.server :as server])
  (:import [java.time Instant LocalTime ZoneId ZonedDateTime]
           [java.util.concurrent ScheduledExecutorService TimeUnit]))

(deftest statistics-queries-are-portable-and-fully-aggregated
  (let [captured (atom [])]
    (with-redefs [db/query-db (fn [query] (swap! captured conj query) [])]
      (server/mime-type-statistics)
      (server/language-statistics)
      (server/category-statistics))
    (let [sql (mapv #(first (honey/format % {:inline true})) @captured)]
      (is (= 3 (count sql)))
      (is (every? #(str/includes? % "COUNT(headers.message_id)") sql))
      (is (every? #(not (re-find #"(?i)\binterval\b" %)) sql))
      (is (str/includes? (second sql) "LEFT JOIN metadata"))
      (is (str/includes? (nth sql 2) "LEFT JOIN categories")))))

(deftest yearly-statistics-query-does-not-use-a-reserved-alias
  (doseq [[mariadb? expected-bucket]
          [[true "YEAR(FROM_UNIXTIME(date))"]
           [false "STRFTIME('%Y', DATETIME(date, 'unixepoch'))"]]]
    (with-redefs [db/mariadb? (constantly mariadb?)]
      (let [sql (first (honey/format (db/email-statistics-query :yearly) {:inline true}))]
        (is (str/includes? sql (str expected-bucket " AS time_bucket")))
        (is (str/includes? sql (str "GROUP BY " expected-bucket)))
        (is (not (re-find #"(?i)\bAS\s+interval\b" sql))))))
  "MariaDB and SQLite aggregate by year in SQL instead of returning one row per timestamp")

(defn- ok-handler [_] {:status 200 :body "secret"})

(deftest automatic-training-retries-after-a-failed-run-and-does-not-start-twice
  (let [attempts (atom 0)
        completed (promise)
        executor (atom nil)]
    (try
      (with-redefs [preferences/record-successful-training! (fn [_])
                    server/write-emails-to-training-files-and-train
                    (fn []
                      (if (= 1 (swap! attempts inc))
                        (throw (ex-info "simulated training failure" {}))
                        (do (deliver completed true)
                            nil)))]
        (let [^ScheduledExecutorService first-executor
              (server/start-training-scheduler! 0 10 TimeUnit/MILLISECONDS)
              second-executor (server/start-training-scheduler! 0 10 TimeUnit/MILLISECONDS)]
          (reset! executor first-executor)
          (is (identical? first-executor second-executor)
              "Starting the scheduler twice reuses the existing schedule")
          (is (true? (deref completed 1000 false))
              "A failed automatic run does not prevent the next training attempt")
          (is (<= 2 @attempts))))
      (finally
        (server/stop-training-scheduler!)
        (when-let [^ScheduledExecutorService ex @executor]
          (is (.isShutdown ex) "Stopping Plauna also shuts down automatic training")))))
  "Automatic training remains periodic after errors and has only one scheduler")

(deftest daily-training-schedule-uses-a-wall-clock-time-and-catches-up-missed-runs
  (let [zone (ZoneId/of "Europe/Berlin")
        time (LocalTime/parse "02:00")
        before (ZonedDateTime/of 2026 8 7 1 0 0 0 zone)
        after (ZonedDateTime/of 2026 8 7 3 0 0 0 zone)
        successful-today (Instant/parse "2026-08-07T00:05:00Z")
        successful-yesterday (Instant/parse "2026-08-06T00:05:00Z")]
    (is (= (* 60 60 1000)
           (server/daily-training-delay-millis before time nil))
        "Before today's time, a new installation waits until that time")
    (is (zero? (server/daily-training-delay-millis after time nil))
        "After today's time, a missing first run starts immediately")
    (is (= (* 23 60 60 1000)
           (server/daily-training-delay-millis after time successful-today))
        "A completed daily run schedules tomorrow at the configured time")
    (is (zero? (server/daily-training-delay-millis after time successful-yesterday))
        "A restart catches up when today's run was missed")))

(deftest categorization-model-switch-requires-the-exact-confirmation
  (let [calls (atom [])]
    (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                  server/train-categorization-model! (fn [model] (swap! calls conj [:train model]))
                  preferences/update-preference (fn [& args] (swap! calls conj [:update args]))]
      (let [result (server/switch-categorization-model!
                    {:model "maxent" :use-current-categories "true" :confirmation "start training"})]
        (is (= :alert (:type result)))
        (is (empty? @calls) "Neither training nor the preference changes without exact confirmation")))))

(deftest categorization-model-switch-trains-before-activating-the-target
  (let [calls (atom [])]
    (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                  server/train-categorization-model! (fn [model] (swap! calls conj [:train model]) nil)
                  preferences/update-preference (fn [key value] (swap! calls conj [:update key value]))
                  preferences/record-successful-training! (fn [_] (swap! calls conj [:record-success]))]
      (let [result (server/switch-categorization-model!
                    {:model "maxent"
                     :use-current-categories "true"
                     :confirmation server/model-switch-confirmation})]
        (is (= :success (:type result)))
        (is (= [[:train "maxent"]
                [:update :categorization-algorithm "maxent"]
                [:record-success]]
               @calls)
            "The active setting changes only after target training completed")))))

(deftest failed-model-migration-keeps-the-previous-model-active
  (let [updates (atom [])]
    (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                  server/train-categorization-model! (fn [_] (throw (ex-info "training failed" {})))
                  preferences/update-preference (fn [& args] (swap! updates conj args))]
      (let [result (server/switch-categorization-model!
                    {:model "maxent"
                     :use-current-categories "true"
                     :confirmation server/model-switch-confirmation})]
        (is (= :alert (:type result)))
        (is (empty? @updates) "A failed target model is never made active")))))

(deftest first-switch-cannot-skip-training-when-no-target-model-exists
  (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                server/languages-to-use-in-training (fn [] ["deu"])
                files/model-file (fn [_ _] (java.io.File. "/definitely/not/a/plauna/model.bin"))]
    (let [result (server/switch-categorization-model!
                  {:model "maxent"
                   :confirmation server/model-switch-confirmation})]
      (is (= :alert (:type result)))
      (is (str/includes? (:content result) "No complete model")))))

(deftest wrap-authentication-blocks-unauthenticated
  (let [handler (server/wrap-authentication ok-handler)
        response (handler {:uri "/emails" :session {}})]
    (is (= 302 (:status response)) "Unauthenticated request is redirected")
    (is (= "/login" (get-in response [:headers "Location"])) "Redirected to the login page"))
  "Unauthenticated requests to protected paths are redirected to /login")

(deftest wrap-authentication-allows-authenticated
  (let [handler (server/wrap-authentication ok-handler)
        response (handler {:uri "/emails" :session {:authenticated true}})]
    (is (= 200 (:status response)))
    (is (= "secret" (:body response))))
  "A logged-in session can reach protected paths")

(deftest wrap-authentication-promotes-allowlisted-mtls-certificate-to-session
  (with-redefs [auth/mtls-request-authorized? (constantly true)]
    (let [handler  (server/wrap-authentication ok-handler)
          response (handler {:uri "/emails"
                             :session {:oauth-csrf "keep-me"}
                             :headers {}})]
      (is (= 200 (:status response)))
      (is (= "secret" (:body response)))
      (is (= {:oauth-csrf "keep-me" :authenticated true} (:session response))
          "The signed browser session is established without discarding other session state")))
  "An allowlisted client certificate bypasses the password and creates a normal session")

(deftest wrap-authentication-does-not-trust-unapproved-mtls-request
  (with-redefs [auth/mtls-request-authorized? (constantly false)]
    (let [response ((server/wrap-authentication ok-handler)
                    {:uri "/emails" :session {} :headers {}})]
      (is (= 302 (:status response)))
      (is (= "/login" (get-in response [:headers "Location"])))))
  "Failed or incomplete proxy assertions still require the Plauna password")

(deftest mtls-admin-post-saves-and-activates-the-submitted-settings
  (let [submitted (atom nil)]
    (try
      (with-redefs [auth/save-mtls-settings-from-request!
                    (fn [request] (reset! submitted request) {:enabled true})]
        (let [response ((server/make-routes {})
                        {:request-method :post
                         :uri "/admin/mtls"
                         :params {:trusted-cert-sha256 "fingerprint"
                                  :proxy-secret "secret"
                                  :current-password "admin-password"
                                  :redirect-url "/admin/mtls"}
                         :session {:authenticated true}})]
          (is (= "fingerprint" (get-in @submitted [:params :trusted-cert-sha256])))
          (is (= "secret" (get-in @submitted [:params :proxy-secret])))
          (is (= "admin-password" (get-in @submitted [:params :current-password])))
          (is (= 303 (:status response)))
          (is (= "/admin/mtls" (get-in response [:headers "Location"])))))
      (finally
        (server/empty-global-messages))))
  "Saving through the admin endpoint uses PRG and activates the settings immediately")

(deftest password-login-requires-the-configured-login-name
  (with-redefs [auth/verify-web-credentials? #(and (= "alice" %1) (= "admin-password" %2))
                auth/web-login-name (constantly "alice")]
    (let [valid-response ((server/make-routes {})
                          {:request-method :post
                           :uri "/login"
                           :params {:login-name "alice" :password "admin-password"}
                           :session {}})
          invalid-response ((server/make-routes {})
                            {:request-method :post
                             :uri "/login"
                             :params {:login-name "root" :password "admin-password"}
                             :session {}})]
      (is (= 302 (:status valid-response)))
      (is (true? (get-in valid-response [:session :authenticated])))
      (is (= 200 (:status invalid-response)))
      (is (str/includes? (:body invalid-response) "Invalid login name or password."))))
  "A correct password cannot authenticate under a different login name")

(deftest login-name-change-requires-the-current-password
  (let [saved (atom nil)]
    (with-redefs [auth/verify-web-password? #(= "admin-password" %)
                  auth/set-login-name! #(reset! saved %)]
      (let [response ((server/make-routes {})
                      {:request-method :post
                       :uri "/admin/login-name"
                       :params {:login-name "alice"
                                :current-password "admin-password"
                                :redirect-url "/admin"}
                       :session {:authenticated true}})]
        (is (= "alice" @saved))
        (is (= 303 (:status response)))))
    (reset! saved nil)
    (with-redefs [auth/verify-web-password? (constantly false)
                  auth/set-login-name! #(reset! saved %)]
      ((server/make-routes {})
       {:request-method :post
        :uri "/admin/login-name"
        :params {:login-name "attacker" :current-password "wrong" :redirect-url "/admin"}
        :session {:authenticated true}})
      (is (nil? @saved)))
    (server/empty-global-messages))
  "An authenticated session alone cannot rename the web login")

(deftest emails-parameters-tolerate-blank-numbers
  ;; The page-size field is a free-form number input; clearing it submits size= (empty string).
  ;; That must fall back to the default instead of throwing NumberFormatException and 400-ing
  ;; the whole request.
  (let [parse-fn (server/template->request-parameters server/emails-template)]
    (is (= 20 (:size (parse-fn {:size ""}))) "A blank size falls back to the default")
    (is (= 20 (:size (parse-fn {:size "abc"}))) "A non-numeric size falls back to the default")
    (is (= 50 (:size (parse-fn {:size "50"}))) "A valid size is parsed")
    (is (= 1 (:page (parse-fn {:page ""}))) "A blank page falls back to the default")
    (is (= "true" (:subject-values-none (parse-fn {:subject-values-none "true"})))
        "An explicitly empty checklist survives request parsing"))
  "Blank or non-numeric size/page parameters fall back to their defaults")

(deftest wrap-authentication-allows-public-paths
  (let [handler (server/wrap-authentication ok-handler)]
    (doseq [uri ["/login" "/css/tailwind.css" "/favicon-32x32.png"
                 "/plauna-banner.png" "/site.webmanifest"]]
      (is (= 200 (:status (handler {:uri uri :session {}})))
          (str uri " is reachable without authentication"))))
  "Login and static assets are reachable without authentication")

(deftest static-assets-are-revalidated-after-a-deployment
  (let [handler (server/wrap-static-asset-revalidation
                 (fn [_] {:status 200 :headers {"Content-Type" "text/css"} :body "asset"}))]
    (is (= "no-cache" (get-in (handler {:uri "/css/tailwind.css"}) [:headers "Cache-Control"])))
    (is (= "no-cache" (get-in (handler {:uri "/js/vendor/vega.min.js"}) [:headers "Cache-Control"])))
    (is (nil? (get-in (handler {:uri "/statistics"}) [:headers "Cache-Control"]))))
  "Stable asset URLs must not leave browsers on an old UI after a container update")

(deftest recategorize-email-reports-a-missing-imap-message-without-saving
  (let [updates (atom [])]
    (with-redefs [server/enriched-email-by-message-id (fn [_] {:header {:message-id "missing-1"}})
                  db/get-categories (fn [] [{:id 7 :name "Correct"}])
                  app/move-email-to-category (fn [_ _ _] {:result :not-found})
                  db/update-metadata-category (fn [& args] (swap! updates conj args))]
      (let [response (server/recategorize-email-response {} {:message-id "missing-1" :category "7"})]
        (is (= 404 (:status response)))
        (is (= "email-not-found" (:body response)))
        (is (empty? @updates) "The category is not changed before the user confirms")))))

(deftest recategorize-email-force-updates-only-plauna-metadata
  (let [updates (atom [])]
    (with-redefs [app/move-email-to-category (fn [& _] (throw (ex-info "IMAP must not be called" {})))
                  db/update-metadata-category (fn [& args] (swap! updates conj args))]
      (let [response (server/recategorize-email-response {} {:message-id "missing-1" :category "7" :force "true"})]
        (is (= 204 (:status response)))
        (is (= [["missing-1" 7 1.0]] @updates))))))

(deftest recategorize-email-keeps-general-move-errors-non-overridable
  (let [updates (atom [])]
    (with-redefs [server/enriched-email-by-message-id (fn [_] {:header {:message-id "error-1"}})
                  db/get-categories (fn [] [{:id 7 :name "Correct"}])
                  app/move-email-to-category (fn [_ _ _] {:result :error})
                  db/update-metadata-category (fn [& args] (swap! updates conj args))]
      (let [response (server/recategorize-email-response {} {:message-id "error-1" :category "7"})]
        (is (= 200 (:status response)))
        (is (= "saved-not-moved" (:body response)))
        (is (empty? @updates))))))

(deftest reconnect-control-restarts-the-connection
  (let [calls (atom [])
        existing-connection {:id "conn-1"}]
    (with-redefs [client/connection-data-from-id (fn [id]
                                                   (swap! calls conj [:lookup id])
                                                   existing-connection)
                  client/disconnect (fn [connection-data]
                                      (swap! calls conj [:disconnect connection-data]))
                  app/connect-to-client (fn [_ id]
                                          (swap! calls conj [:connect id])
                                          {:result :ok})]
      (let [response (#'server/reconnect-control-response {} {:uri "/admin/connections"
                                                               :params {}
                                                               :session {}}
                      "conn-1")]
        (is (= [[:lookup "conn-1"]
                [:disconnect existing-connection]
                [:connect "conn-1"]]
               @calls))
        (is (= 303 (:status response)))
        (is (= "/admin/connections" (get-in response [:headers "Location"])))))))

(deftest blank-secrets-keep-the-values-already-stored
  (let [connection (#'server/connection-update-from-params
                    {:id "connection-1"
                     :host "imap.example.com"
                     :user "me@example.com"
                     :secret ""
                     :folder "INBOX"
                     :security "ssl"
                     :auth-type "basic"}
                    {:secret "stored-imap-secret"})
        provider (#'server/auth-provider-update-from-params
                  {:id "12" :name "Example" :client_secret ""}
                  {:client-secret "stored-oauth-secret"})]
    (is (= "stored-imap-secret" (:secret connection)))
    (is (= "stored-oauth-secret" (:client_secret provider)))
    (is (= "new-secret"
           (:secret (#'server/connection-update-from-params
                     {:id "connection-1" :secret "new-secret" :security "ssl" :auth-type "basic"}
                     {:secret "stored-imap-secret"}))))
    (is (= "new-client-secret"
           (:client_secret (#'server/auth-provider-update-from-params
                            {:id "12" :client_secret "new-client-secret"}
                            {:client-secret "stored-oauth-secret"}))))))

(deftest connection-template-context-excludes-the-stored-imap-secret
  (with-redefs [db/get-connection (fn [_] {:id "connection-1"
                                           :host "imap.example.com"
                                           :secret "must-not-enter-template-context"})
                client/monitor->map (fn [_] {:connected false})]
    (let [connection (server/connection-information "connection-1")]
      (is (= "imap.example.com" (:host connection)))
      (is (not (contains? connection :secret))))))

(deftest oauth-start-stores-only-the-provider-id-in-the-browser-session
  (let [provider {:id 12
                  :name "Example"
                  :auth-url "https://login.example.com/authorize"
                  :client-id "client-id"
                  :client-secret "must-stay-on-server"
                  :redirect-url "https://plauna.example.com/oauth2/callback"
                  :scope "mail"}]
    (with-redefs [app/connect-to-client (fn [_ _] {:result :redirect :provider provider})]
      (let [response (#'server/connect-control-response
                      {}
                      {:session {:authenticated true
                                 :provider {:client-secret "old-secret-from-an-existing-session"}}}
                      "connection-1")
            session (:session response)]
        (is (= 302 (:status response)))
        (is (= 12 (:provider-id session)))
        (is (not (contains? session :provider)))
        (is (not (str/includes? (pr-str session) "must-stay-on-server")))))))

(deftest oauth-callback-loads-provider-server-side-and-clears-one-time-session-data
  (let [provider {:id 12 :client-secret "server-side-secret"}
        exchanged (atom nil)]
    (with-redefs [db/get-auth-provider (fn [id]
                                        (is (= 12 id))
                                        provider)
                  oauth/exchange-code-for-access-token
                  (fn [loaded-provider code]
                    (reset! exchanged [loaded-provider code])
                    {:access_token "token"})
                  db/save-oauth-token (fn [_])
                  app/connect-to-client (fn [_ _] {:result :ok})]
      (let [response ((server/make-routes {})
                      {:request-method :get
                       :uri "/oauth2/callback"
                       :params {:state "csrf" :code "authorization-code"}
                       :session {:authenticated true
                                 :oauth-csrf "csrf"
                                 :connection-id "connection-1"
                                 :provider-id 12}})]
        (is (= [provider "authorization-code"] @exchanged))
        (is (= 302 (:status response)))
        (is (= {:authenticated true} (:session response)))))))

(deftest refetch-email-saves-participants
  (let [participants [{:message-id "msg-1" :contact-key "sender-key" :name "Sender" :address "sender@example.com" :type :sender}
                      {:message-id "msg-1" :contact-key "to-key" :name "Receiver" :address "to@example.com" :type :receiver}
                      {:message-id "msg-1" :contact-key "cc-key" :name "Copy" :address "cc@example.com" :type :cc}
                      {:message-id "msg-1" :contact-key "bcc-key" :name "Blind" :address "bcc@example.com" :type :bcc}]
        calls (atom [])]
    (with-redefs [client/refetch-message-by-id (fn [_] {:body [] :participants participants})
                  db/fetch-bodies-for (fn [_] [])
                  db/save-contacts (fn [contacts] (swap! calls conj [:contacts contacts]))
                  db/save-communications (fn [contacts] (swap! calls conj [:communications contacts]))
                  db/fetch-metadata (fn [_] {:language "en"})]
      (is (= {:type :success :content "Re-fetched the email from the server and filled in its contents."}
             (server/refetch-email-and-fill! "msg-1")))
      (is (= [[:contacts participants]
              [:communications participants]]
             @calls)))))
