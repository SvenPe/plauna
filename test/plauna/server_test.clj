(ns plauna.server-test
  (:require [clojure.test :refer :all]
            [plauna.application :as app]
            [plauna.client :as client]
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
