(ns plauna.server-test
  (:require [clojure.test :refer :all]
            [plauna.application :as app]
            [plauna.client :as client]
            [plauna.database :as db]
            [plauna.server :as server]))

(defn- ok-handler [_] {:status 200 :body "secret"})

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
    (is (= 1 (:page (parse-fn {:page ""}))) "A blank page falls back to the default"))
  "Blank or non-numeric size/page parameters fall back to their defaults")

(deftest wrap-authentication-allows-public-paths
  (let [handler (server/wrap-authentication ok-handler)]
    (doseq [uri ["/login" "/css/tailwind.css" "/favicon-32x32.png"
                 "/plauna-banner.png" "/site.webmanifest"]]
      (is (= 200 (:status (handler {:uri uri :session {}})))
          (str uri " is reachable without authentication"))))
  "Login and static assets are reachable without authentication")

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
