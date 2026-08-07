(ns plauna.server-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [plauna.application :as app]
            [plauna.client :as client]
            [plauna.client.oauth :as oauth]
            [plauna.database :as db]
            [plauna.server :as server])
  (:import [java.util.concurrent ScheduledExecutorService TimeUnit]))

(defn- ok-handler [_] {:status 200 :body "secret"})

(deftest automatic-training-retries-after-a-failed-run-and-does-not-start-twice
  (let [attempts (atom 0)
        completed (promise)
        executor (atom nil)]
    (try
      (with-redefs [server/write-emails-to-training-files-and-train
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
