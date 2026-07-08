(ns plauna.application-test
  (:require [clojure.test :refer :all]
            [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [plauna.application :as app]))

(t/set-ns-filter! {:disallow "plauna.*"})

(deftest basic-auth
  (let [database (reify int/DB (fetch-connection [_ id] {:id id :auth-type "basic"}))
        client (reify int/EmailClient (start-monitor [_ config context]))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "Basic authentication calls email-client's login method and returns ok")))

(deftest basic-auth-2
  (let [database (reify int/DB (fetch-connection [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context]))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "If no auth-type is defined, fall back on basic auth and return ok")))

(deftest oauth2-auth
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context]))
        context {:db database :client client}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider but no token data returns a :redirect with the provider")))

(deftest oauth2-auth-2
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty" :refresh-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context]))
        context {:db database :client client}]
    (is (= {:result :ok}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider and token data calls client login and returns ok")))

(deftest oauth2-auth-3
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] nil))
        client (reify int/EmailClient (start-monitor [_ config context]))
        context {:db database :client client}]
    (is (= :error (:result (app/connect-to-client context "abc"))))
    "auth-type 'oauth2' with no auth provider returns an errorq"))

(deftest oauth2-auth-4
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context]))
        context {:db database :client client}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider and access token but no refresh token calls client login and returns ok")))

(defn- stub-emails-db
  "A reify int/DB with every method fetch-emails needs beyond fetch-emails itself: the three
   distinct-value lookups and fetch-header-categories (all empty by default, ignoring whatever
   other-filters-where they're called with) plus fetch-categories. capture-fn receives the
   customization clause passed to fetch-emails; result is what fetch-emails should return."
  [capture-fn result]
  (reify int/DB
    (fetch-categories [_] [])
    (fetch-distinct-subjects [_ _] [])
    (fetch-distinct-senders [_ _] [])
    (fetch-distinct-recipients [_ _] [])
    (fetch-header-categories [_ _] [])
    (fetch-emails [_ _ customization] (capture-fn customization) result)))

(deftest emails-query-filter-wo-search
  (let [query (atom "")
        database (stub-emails-db #(reset! query %) {:total 10 :size 1 :page 1})]
    (app/fetch-emails {:db database} {:filter "enriched-only" :size 1})
    (is (= @query {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]], :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:filter "without-category" :size 1})
    (is (= @query {:where [:= :metadata.category nil] :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:size 1})
    (is (= {:order-by [[:date :desc]]} @query))))

(deftest emails-query-search-wo-filter
  (let [query (atom "")
        database (stub-emails-db #(reset! query %) {:total 10 :size 1 :page 1})]
    (app/fetch-emails {:db database} {:search-text "test text" :size 1})
    (is (= {:where [:in :headers.message-id
                    {:select [:bodies.message-id] :from [:bodies] :where [:like :bodies.content [:escape "%test text%" "\\"]]}]
            :order-by [[:date :desc]]}
           @query))))

(deftest emails-query-search-filter
  (let [query (atom "")
        database (stub-emails-db #(reset! query %) {:total 10 :size 1 :page 1})]
    (app/fetch-emails {:db database} {:filter "enriched-only" :search-text "test text" :size 1})
    (is (= {:where [:and [:and [:<> :metadata.category nil] [:<> :metadata.language nil]]
                    [:in :headers.message-id
                     {:select [:bodies.message-id] :from [:bodies] :where [:like :bodies.content [:escape "%test text%" "\\"]]}]]
            :order-by [[:date :desc]]}
           @query))))

(deftest create-a-category
  (let [db-called (atom false)
        client-called (atom false)
        database (reify int/DB (save-category [_ _ _ _] (swap! db-called (fn [_] true))))
        client (reify int/EmailClient
                 (connections [_] {"does not matter" "some-data"})
                 (create-category-directories! [_ _ _] (swap! client-called (fn [_] true))))]
    (app/create-new-category! {:db database :client client} "test" nil nil)
    (is (= true @db-called))
    (is (= true @client-called)))
  "Creating a new category makes correct database and client calls")

(deftest create-category-passes-through-a-valid-color
  (let [captured-color (atom :not-called)
        database (reify int/DB (save-category [_ _ _ color] (reset! captured-color color)))
        client (reify int/EmailClient
                 (connections [_] {})
                 (create-category-directories! [_ _ _] nil))]
    (app/create-new-category! {:db database :client client} "test" nil "#3b82f6")
    (is (= "#3b82f6" @captured-color)))
  "A valid hex color is saved as-is")

(deftest create-category-drops-an-invalid-color
  (let [captured-color (atom :not-called)
        database (reify int/DB (save-category [_ _ _ color] (reset! captured-color color)))
        client (reify int/EmailClient
                 (connections [_] {})
                 (create-category-directories! [_ _ _] nil))]
    (app/create-new-category! {:db database :client client} "test" nil "javascript:alert(1)")
    (is (nil? @captured-color))
    (app/create-new-category! {:db database :client client} "test" nil "")
    (is (nil? @captured-color)))
  "An invalid or blank color is saved as nil rather than trusted verbatim")

(deftest update-category-passes-through-destination-and-color
  (let [captured (atom nil)
        database (reify int/DB (update-category [_ id destination-folder color] (reset! captured [id destination-folder color])))
        client (reify int/EmailClient
                 (connections [_] {})
                 (create-category-directories! [_ _ _] nil))]
    (app/update-category! {:db database :client client} "7" "test" "Archive/Test" "#ff0000")
    (is (= ["7" "Archive/Test" "#ff0000"] @captured)))
  "update-category! forwards the trimmed destination folder and the validated color")

(deftest move-email-without-connections
  (let [client (reify int/EmailClient
                 (connections [_] {})
                 (connection-id-for-email [_ _ _] nil))
        test-result (app/move-email-to-category {} "test" {:client client})]
    (is (= :error (:result test-result))))
  "If there are no connections, just moving email returns an error result.")

(deftest move-email-with-guessed-connection-id-success
  (let [client (reify int/EmailClient
                 (connections [_] {"test" {}})
                 (connection-id-for-email [_ _ _] "test")
                 (move-email-between-categories [_ _ _ _ _ _] true))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :ok (:result test-result))))
  "Successfully moving an email with a guessed connection id returns result :ok")

(deftest move-email-with-guessed-connection-id-error
  (let [client (reify int/EmailClient
                 (connections [_] {"test" {}})
                 (connection-id-for-email [_ _ _] "test")
                 (move-email-between-categories [_ _ _ _ _ _] false))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :error (:result test-result))))
  "Unsuccessfully moving an email with a guessed connection id returns result :error")

(deftest move-email-without-guessed-connection-id-success
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ id _ _ _ _] (= id "test2")))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :ok (:result test-result))))
  "Successfully moving an email without guessed connection id returns result :ok even if the process failed in some other connection.")

(deftest move-email-without-guessed-connection-id-error
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ id _ _ _ _] (= id "test3")))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :error (:result test-result))))
  "Unsuccessfully moving an email without guessed connection id returns result :error")

(deftest handle-incoming-email-analyzer-exception
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {} {:analyzer analyzer})]
    (is (= :error (:result test-result))))
  "Return an error result if something goes wrong with the analyzer")

(deftest handle-incoming-email-db-exception
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] "test"))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {} {:analyzer analyzer :db db})]
    (is (= :error (:result test-result))))
  "Return an error result of something goes wrong with the database")

(deftest handle-incoming-email-client-exception
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? true} {:analyzer analyzer :db db :client client})]
    (is (= :error (:result test-result))))
  "Return error if move=true and something goes wrong in the client")

(deftest handle-incoming-email-client-exception-move-false
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? false} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result))))
  "Client is not called if move=false")

(deftest handle-incoming-email-client-exception-move-true
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category nil}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? true} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result))))
  "Client is not called if move=true but not category")

(deftest handle-incoming-email-happy-path
  (let [recorded (atom nil)
        analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true)
             (update-email-folder [_ message-id folder] (reset! recorded [message-id folder])))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] "Categories/Test"))
        test-result (app/handle-incoming-imap-email {:header {:message-id "happy-1"}} {:move? true} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (= ["happy-1" "Categories/Test"] @recorded) "A successful move records the destination folder"))
  "Happy path. All underlying functions are called normally. Return an :ok result.")

(deftest handle-incoming-email-failed-move-records-source-folder
  (let [recorded (atom nil)
        analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true)
             (update-email-folder [_ message-id folder] (reset! recorded [message-id folder])))
        client (reify int/EmailClient
                 (move-email-to-category [_ _ _ _ _] nil)         ; move did not complete (e.g. copy fallback failed)
                 (current-folder-name [_ folder] (str folder)))
        test-result (app/handle-incoming-imap-email {:header {:message-id "fail-1"}} {:move? true :origin-folder :inbox} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (= ["fail-1" ":inbox"] @recorded) "A failed move records the email's actual (source) folder, not the destination"))
  "When a move does not complete, the email stays in its source folder and that folder is recorded so later recategorization can still find it.")

(deftest handle-incoming-email-skips-existing-email
  (let [analyzer (reify int/Analyzer
                   (enrich-email [_ _] (throw (ex-info "existing email should not be enriched" {})))
                   (detect-language [_ _] (throw (ex-info "existing email should not be language-detected" {}))))
        db (reify int/DB
             (email-exists? [_ message-id] (= "known-1" message-id))
             (save-email [_ _] (throw (ex-info "existing email should not be saved" {})))
             (update-email-folder [_ _ _] (throw (ex-info "existing email folder should not be updated" {}))))
        client (reify int/EmailClient
                 (move-email-to-category [_ _ _ _ _] (throw (ex-info "existing email should not be moved" {}))))
        test-result (app/handle-incoming-imap-email {:header {:message-id "known-1"}}
                                                    {:move? true :origin-folder :spam :message :message}
                                                    {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (nil? (:move test-result))))
  "Existing emails are not re-categorized, re-saved, or moved when a folder parse sees them again.")

(deftest read-emails-from-folder-continues-after-read-exception
  (let [read-attempts (atom [])
        saved-subjects (atom [])
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _]
                   {:message-count 3 :connection-id "test-connection" :folder :test-folder})
                 (close-folder-for-bulk-read [_ _] nil)
                 (current-folder-name [_ folder] (str folder))
                 (nth-email-from-folder [_ n _]
                   (swap! read-attempts conj n)
                   (if (= n 2)
                     (throw (ex-info "failed to read message" {:n n}))
                     {:email {:header {:subject (str "email-" n)}} :message n})))
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ email] (swap! saved-subjects conj (-> email :header :subject)))
             (update-email-folder [_ _ _] nil))]
    (is (= 3 (app/read-emails-from-folder {} "Newsletter" {:move? false} {:client client :analyzer analyzer :db db})))
    (let [deadline (+ (System/currentTimeMillis) 1000)]
      (loop []
        (when (and (< (count @saved-subjects) 2)
                   (< (System/currentTimeMillis) deadline))
          (Thread/sleep 10)
          (recur))))
    ;; Sequence numbers are processed high -> low so that moving/expunging a message never shifts the
    ;; numbers of messages not yet processed. Message 2 throws on read and is skipped; 3 and 1 succeed.
    (is (= [3 2 1] @read-attempts))
    (is (= ["email-3" "email-1"] @saved-subjects))))

(deftest fetch-emails-applies-date-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text nil
                                :page 1 :size 20 :date-from "2026-06-01" :date-to "2026-06-30"})
    (let [where-flat (flatten (:where @captured))]
      (is (some #{:>=} where-flat) "Lower date bound is present")
      (is (some #{:<} where-flat) "Upper date bound is present")
      (is (some #{:headers.date} where-flat) "Filters on the date column")))
  "Date-from/date-to are translated into a date-range filter on the email date")

(deftest fetch-emails-applies-content-search
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text "someone@example.com" :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:bodies.message-id] :from [:bodies]
                     :where [:like :bodies.content [:escape "%someone@example.com%" "\\"]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "The top Search Text field matches the e-mail body content, not the subject")

(deftest fetch-emails-escapes-like-wildcards
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text "a_b%c\\d" :page 1 :size 20})
    (let [likes (filter #(and (vector? %) (= :like (first %))) (tree-seq coll? seq (:where @captured)))]
      (is (seq likes) "The content search produces a LIKE clause")
      (doseq [[_ _ [_ pattern escape-char]] likes]
        (is (= "%a\\_b\\%c\\\\d%" pattern) "LIKE wildcards and the escape char itself are escaped")
        (is (= "\\" escape-char) "An explicit ESCAPE character is set (SQLite has no default)"))))
  "User input containing % _ or \\ matches literally instead of acting as LIKE wildcards")

(deftest fetch-emails-no-content-filter-when-blank
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text nil :page 1 :size 20})
    (is (not (contains? @captured :where))))
  "A blank Search Text adds no filter")

(deftest fetch-emails-applies-subject-values-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :subject-values ["Invoice" "Newsletter"] :page 1 :size 20})
    (is (= {:where [:in :headers.subject ["Invoice" "Newsletter"]] :order-by [[:date :desc]]} @captured)))
  "Checked subjects are translated into an IN filter on the subject column")

(deftest fetch-emails-no-subject-values-filter-when-empty
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :subject-values nil :page 1 :size 20})
    (is (not (contains? @captured :where))))
  "No subject checked (the default, before any checkbox is unchecked) adds no filter")

(deftest fetch-emails-applies-subject-values-exclude-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :subject-values-exclude ["Spam"] :page 1 :size 20})
    (is (= {:where [:not-in :headers.subject ["Spam"]] :order-by [[:date :desc]]} @captured)))
  "Excluded subjects match every other subject")

(deftest fetch-emails-applies-from-keys-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :from-keys ["key-1" "key-2"] :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:communications.message-id] :from [:communications]
                     :where [:and [:in :communications.type ["sender" ":sender"]]
                                  [:in :communications.contact-key ["key-1" "key-2"]]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "Checked senders are translated into a message-id semi-join by contact-key")

(deftest fetch-emails-applies-to-keys-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :to-keys ["key-3"] :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:communications.message-id] :from [:communications]
                     :where [:and [:in :communications.type ["receiver" ":receiver"]]
                                  [:in :communications.contact-key ["key-3"]]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "Checked recipients are translated into a message-id semi-join by contact-key, using the receiver type")

(deftest fetch-emails-applies-from-keys-exclude-filter
  ;; Unchecking a couple of senders out of hundreds still leaves hundreds checked; the checklist UI
  ;; submits the (few) unchecked ones under from-keys-exclude instead, so the query stays short.
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :from-keys-exclude ["key-1"] :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:communications.message-id] :from [:communications]
                     :where [:and [:in :communications.type ["sender" ":sender"]]
                                  [:not-in :communications.contact-key ["key-1"]]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "Excluded senders are translated into a message-id semi-join matching every other sender")

(deftest fetch-emails-applies-to-keys-exclude-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :to-keys-exclude ["key-3"] :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:communications.message-id] :from [:communications]
                     :where [:and [:in :communications.type ["receiver" ":receiver"]]
                                  [:not-in :communications.contact-key ["key-3"]]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "Excluded recipients are translated into a message-id semi-join matching every other recipient")

(deftest fetch-emails-from-keys-include-wins-over-exclude
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :from-keys ["key-1"] :from-keys-exclude ["key-2"] :page 1 :size 20})
    (let [where (:where @captured)]
      (is (some #{[:in :communications.contact-key ["key-1"]]} (tree-seq coll? seq where)))))
  "If both are somehow present, include takes precedence (the UI only ever sends one)")

(deftest fetch-emails-clamps-page-size
  (let [captured (atom nil)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ entity-clause _] (reset! captured entity-clause) {:data [] :total 0}))]
    (app/fetch-emails {:db db} {:filter "all" :page 1 :size 0})
    (is (= 1 (:size (:page @captured))) "A size of 0 is clamped up to 1, never an invalid LIMIT")
    (app/fetch-emails {:db db} {:filter "all" :page 1 :size 100000})
    (is (= 500 (:size (:page @captured))) "An unbounded size is capped at 500"))
  "fetch-emails clamps a free-form page size into a safe range")

(deftest fetch-emails-applies-category-ids-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["2" "3"] :page 1 :size 20})
    (is (= {:where [:in :metadata.category [2 3]] :order-by [[:date :desc]]} @captured)))
  "Selected numeric category ids are translated into an IN filter")

(deftest fetch-emails-applies-uncategorized-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids [app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:= :metadata.category nil] :order-by [[:date :desc]]} @captured)))
  "Selecting only the 'n/a' checkbox filters to e-mails with no category")

(deftest fetch-emails-applies-mixed-category-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["1" app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:or [:in :metadata.category [1]] [:= :metadata.category nil]] :order-by [[:date :desc]]} @captured)))
  "Selecting both real categories and 'n/a' ORs the two conditions together")

(deftest fetch-emails-no-category-filter-when-empty
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids nil :page 1 :size 20})
    (is (not (contains? @captured :where))))
  "No category-ids selected (the default, before any checkbox is unchecked) adds no filter")

(deftest fetch-emails-applies-category-ids-exclude-filter
  ;; The checklist UI submits an -exclude param instead of -ids once more than half the checkboxes
  ;; are checked, so it never has to send hundreds of ids just to exclude a couple.
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude ["2" "3"] :page 1 :size 20})
    (is (= {:where [:or [:not-in :metadata.category [2 3]] [:= :metadata.category nil]] :order-by [[:date :desc]]} @captured)))
  "Excluded category ids match everything else, including uncategorized e-mails")

(deftest fetch-emails-applies-category-ids-exclude-uncategorized-only
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude [app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:<> :metadata.category nil] :order-by [[:date :desc]]} @captured)))
  "Excluding only 'n/a' requires a real (non-null) category")

(deftest fetch-emails-applies-mixed-category-exclude-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude ["1" app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:and [:<> :metadata.category nil] [:not-in :metadata.category [1]]] :order-by [[:date :desc]]} @captured)))
  "Excluding both a real category and 'n/a' requires a non-null, non-excluded category")

(deftest fetch-emails-category-include-wins-over-exclude
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["1"] :category-ids-exclude ["2"] :page 1 :size 20})
    (is (= {:where [:in :metadata.category [1]] :order-by [[:date :desc]]} @captured)))
  "If both are somehow present, include takes precedence (the UI only ever sends one)")

(deftest fetch-emails-marks-selected-categories-checked
  (let [db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :category-ids ["1"] :page 1 :size 20})
        by-name (into {} (map (juxt :name :checked?)) (:categories (:optional result)))]
    (is (= {"Work" true "Personal" false "n/a" false} by-name)))
  "Only the selected category (and not the others, including n/a) is marked checked")

(deftest fetch-emails-marks-every-category-checked-when-unfiltered
  (let [db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :page 1 :size 20})
        checked-flags (map :checked? (:categories (:optional result)))]
    (is (every? true? checked-flags)))
  "Before any checkbox is unchecked, every category (including n/a) shows as checked")

(deftest fetch-emails-marks-categories-checked-in-exclude-mode
  (let [db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude ["1"] :page 1 :size 20})
        by-name (into {} (map (juxt :name :checked?)) (:categories (:optional result)))]
    (is (= {"Work" false "Personal" true "n/a" true} by-name)))
  "In exclude mode, every category is checked except the excluded one")

(deftest fetch-emails-marks-selected-subjects-senders-and-recipients-checked
  (let [db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [{:subject "Invoice"} {:subject "Newsletter"}])
             (fetch-distinct-senders [_ _] [{:contact_key "s1" :name "Alice" :address "alice@example.com"}
                                          {:contact_key "s2" :name "Bob" :address "bob@example.com"}])
             (fetch-distinct-recipients [_ _] [{:contact_key "r1" :name "Me" :address "me@example.com"}])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :subject-values ["Invoice"] :from-keys ["s2"] :page 1 :size 20})
        optional (:optional result)]
    (is (= {"Invoice" true "Newsletter" false} (into {} (map (juxt :subject :checked?)) (:subjects optional))))
    (is (= {"s1" false "s2" true} (into {} (map (juxt :contact_key :checked?)) (:senders optional))))
    (is (every? true? (map :checked? (:recipients optional))) "Recipients are untouched, so all show as checked"))
  "Each column filter's checked state is independent of the others")

(deftest fetch-emails-marks-senders-checked-in-exclude-mode
  (let [db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [{:contact_key "s1" :name "Alice" :address "alice@example.com"}
                                          {:contact_key "s2" :name "Bob" :address "bob@example.com"}])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :from-keys-exclude ["s1"] :page 1 :size 20})
        by-key (into {} (map (juxt :contact_key :checked?)) (:senders (:optional result)))]
    (is (= {"s1" false "s2" true} by-key)))
  "In exclude mode, every sender is checked except the excluded one")

(deftest fetch-emails-scopes-from-checklist-to-other-active-filters
  ;; The From checklist's possible values must reflect every OTHER active filter (here, category),
  ;; but never the From filter's own selection - otherwise picking a sender would immediately narrow
  ;; the From dropdown down to just that one sender.
  (let [captured-where (atom :not-called)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ where] (reset! captured-where where) [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["1"] :from-keys ["some-key"] :page 1 :size 20})
    (is (= [:in :metadata.category [1]] @captured-where)
        "Scoped to the category filter only; the From filter itself is not applied to its own checklist"))
  "Each checklist's possible-values query excludes its own filter but keeps every other active one")

(deftest fetch-emails-scopes-subject-checklist-to-other-active-filters
  (let [captured-where (atom :not-called)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ where] (reset! captured-where where) [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))]
    (app/fetch-emails {:db db} {:filter "all" :from-keys ["some-key"] :subject-values ["Invoice"] :page 1 :size 20})
    (let [flat (tree-seq coll? seq @captured-where)]
      (is (some #{"some-key"} flat) "The From filter is applied")
      (is (not (some #{"Invoice"} flat)) "The Subject filter's own selection is not applied to its own checklist")))
  "The Subject checklist is scoped by From but not by its own selection")

(deftest fetch-emails-scopes-category-checklist-to-other-active-filters
  (let [captured-where (atom :not-called)
        db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ where] (reset! captured-where where) [{:category 1}])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :from-keys ["alice-key"] :page 1 :size 20})]
    (is (= [:in :headers.message-id
            {:select [:communications.message-id] :from [:communications]
             :where [:and [:in :communications.type ["sender" ":sender"]] [:in :communications.contact-key ["alice-key"]]]}]
           @captured-where)
        "The category checklist's reachable set is scoped by the From filter")
    (is (= ["Work"] (mapv :name (:category-filter-options (:optional result))))
        "Only categories fetch-header-categories reports as reachable appear in the checklist")
    (is (= #{"Work" "Personal" "n/a"} (into #{} (map :name) (:categories (:optional result))))
        "The per-row reassignment dropdown (:categories) is never narrowed"))
  "The Category checklist narrows to reachable categories; the per-row assignment dropdown does not")

(deftest fetch-emails-no-date-filter-when-blank
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text nil
                                :page 1 :size 20 :date-from "" :date-to ""})
    (is (not (some #{:>=} (flatten (:where @captured)))) "No date bound is added when both dates are blank"))
  "Blank date inputs add no date filter")

(deftest fetch-emails-combines-all-filters
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "enriched-only" :search-text "hello" :subject-values ["Invoice"]
                                :from-keys ["s1"] :to-keys ["r1"] :category-ids ["1"]
                                :date-from "2026-06-01" :date-to "2026-06-30" :page 1 :size 20})
    (let [where (:where @captured)]
      (is (= :and (first where)) "Every active filter is ANDed together")
      ;; combine-wheres builds one flat [:and ...] of all 7 active clauses; just check each
      ;; filter's signature is present somewhere in the tree rather than pinning the exact shape.
      (let [flat (tree-seq coll? seq where)]
        (is (some #{:metadata.category} flat) "Metadata filter present")
        (is (some #{:bodies.content} flat) "Content search present")
        (is (some #{:headers.subject} flat) "Subject filter present")
        (is (some #{"sender" ":sender"} flat) "From filter present")
        (is (some #{"receiver" ":receiver"} flat) "To filter present")
        (is (some #{:headers.date} flat) "Date filter present"))))
  "All filters (metadata, content search, subject, from, to, category, date) combine with AND")
