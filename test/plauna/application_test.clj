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

(deftest emails-query-filter-wo-search
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:filter "enriched-only" :size 1})
    (is (= @query {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]], :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:filter "without-category" :size 1})
    (is (= @query {:where [:= :metadata.category nil] :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:size 1})
    (is (= {:order-by [[:date :desc]]} @query))))

(deftest emails-query-search-wo-filter
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:search-field "subject" :search-text "test text" :size 1})
    (is (= {:where [:like :headers.subject "%test text%"] :order-by [[:date :desc]]} @query))))

(deftest emails-query-search-filter
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:filter "enriched-only" :search-field "subject" :search-text "test text" :size 1})
    (is (= {:where [:and [:and [:<> :metadata.category nil] [:<> :metadata.language nil]] [:like :headers.subject "%test text%"]] :order-by [[:date :desc]]} @query))))

(deftest create-a-category
  (let [db-called (atom false)
        client-called (atom false)
        database (reify int/DB (save-category [_ _ _] (swap! db-called (fn [_] true))))
        client (reify int/EmailClient
                 (connections [_] {"does not matter" "some-data"})
                 (create-category-directories! [_ _ _] (swap! client-called (fn [_] true))))]
    (app/create-new-category! {:db database :client client} "test" nil)
    (is (= true @db-called))
    (is (= true @client-called)))
  "Creating a new category makes correct database and client calls")

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
        db (reify int/DB (save-email [_ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {} {:analyzer analyzer :db db})]
    (is (= :error (:result test-result))))
  "Return an error result of something goes wrong with the database")

(deftest handle-incoming-email-client-exception
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? true} {:analyzer analyzer :db db :client client})]
    (is (= :error (:result test-result))))
  "Return error if move=true and something goes wrong in the client")

(deftest handle-incoming-email-client-exception-move-false
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? false} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result))))
  "Client is not called if move=false")

(deftest handle-incoming-email-client-exception-move-true
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category nil}}))
        db (reify int/DB (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? true} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result))))
  "Client is not called if move=true but not category")

(deftest handle-incoming-email-happy-path
  (let [recorded (atom nil)
        analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
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
             (save-email [_ _] true)
             (update-email-folder [_ message-id folder] (reset! recorded [message-id folder])))
        client (reify int/EmailClient
                 (move-email-to-category [_ _ _ _ _] nil)         ; move did not complete (e.g. copy fallback failed)
                 (current-folder-name [_ folder] (str folder)))
        test-result (app/handle-incoming-imap-email {:header {:message-id "fail-1"}} {:move? true :origin-folder :inbox} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (= ["fail-1" ":inbox"] @recorded) "A failed move records the email's actual (source) folder, not the destination"))
  "When a move does not complete, the email stays in its source folder and that folder is recorded so later recategorization can still find it.")

(deftest read-emails-from-folder-continues-after-read-exception
  (let [read-attempts (atom [])
        saved-subjects (atom [])
        client (reify int/EmailClient
                 (number-of-messages-in-folder [_ _ _]
                   {:message-count 3 :connection-id "test-connection" :folder :test-folder})
                 (pause-monitoring-for-folder [_ _ _] false)
                 (current-folder-name [_ folder] (str folder))
                 (nth-email-from-folder [_ n _]
                   (swap! read-attempts conj n)
                   (if (= n 2)
                     (throw (ex-info "failed to read message" {:n n}))
                     {:email {:header {:subject (str "email-" n)}} :message n})))
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (save-email [_ email] (swap! saved-subjects conj (-> email :header :subject)))
             (update-email-folder [_ _ _] nil))]
    (is (= 3 (app/read-emails-from-folder {} "Newsletter" {:move? false} {:client client :analyzer analyzer :db db})))
    (let [deadline (+ (System/currentTimeMillis) 1000)]
      (loop []
        (when (and (< (count @saved-subjects) 2)
                   (< (System/currentTimeMillis) deadline))
          (Thread/sleep 10)
          (recur))))
    (is (= [1 2 3] @read-attempts))
    (is (= ["email-1" "email-3"] @saved-subjects))))
