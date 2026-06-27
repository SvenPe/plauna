(ns plauna.entry
  (:require
   [plauna.analysis :as analysis]
   [plauna.application :as app]
   [plauna.auth :as auth]
   [plauna.client :as client]
   [plauna.core.email :as core.email]
   [plauna.core.events :as events]
   [plauna.database :as db]
   [plauna.db-config :as db-cfg]
   [plauna.settings :as settings]
   [plauna.diagnostics :as diagnostics]
   [plauna.files :as files]
   [plauna.messaging :as messaging]
   [plauna.parser :as parser]
   [plauna.preferences :as preferences]
   [plauna.server :as server]
   [taoensso.telemere :as t])
  (:import [plauna.client ImapClient]
           [plauna.database SqliteDB]
           [plauna.analysis BasicAnalyzer])
  (:gen-class))

(defn setup-logging []
  (t/set-min-level! :info)
  ;; jetty is very noisy. Disable all jetty logs.
  (t/set-ns-filter! {:disallow "org.eclipse.jetty.*"})
  (diagnostics/install-uncaught-exception-handler!))

(set! *warn-on-reflection* true)

(def event-register {:enrichment-event-loop (fn [] (analysis/enrichment-event-loop @messaging/main-publisher @messaging/main-chan))
                     ;:client-event-loop (fn [] (client/client-event-loop @messaging/main-publisher))
                     :database-event-loop (fn [] (db/database-event-loop @messaging/main-publisher))
                     :parser-event-loop (fn [] (parser/parser-event-loop @messaging/main-publisher @messaging/main-chan))})

(defn start-imap-client
  [context]
  (let [connections-in-db (db/get-connections)]
    (if (seq connections-in-db)
      (do (t/log! :debug ["Connections table contains" (count connections-in-db) "connection configuration(s)."])
          (doseq [client-config connections-in-db]
            (let [connection-result (app/connect-to-client context (:id client-config))]
              (if (= :ok (:result connection-result))
                (client/create-category-folders! (get @client/connections (:id client-config)) (mapv :name (db/get-categories)))
                (t/log! :info ["Not connected, not creating folders."])))))
      (do (t/log! :debug "Connections table in the db is empty. Trying to read connections from the config file.")
          (doseq [client-config (:clients (-> context :config :email))]
            (t/log! :info ["Adding connection data from the config file to the database. Next time Plauna will use the data from the database."])
            (let [connection-with-id (core.email/construct-imap-connection-from-config-file (conj client-config {:id (client/id-from-config client-config)}))]
              (db/add-connection connection-with-id)
              (let [connection-result (app/connect-to-client context (:id client-config))]
                (if (= :ok (:result connection-result))
                  (client/create-category-folders! (get @client/connections (:id client-config)) (mapv :name (db/get-categories)))
                  (t/log! :info ["Connection failed for config:" client-config]))))))))
  (t/log! :debug "Listening to new emails from listen-channel"))

(defn- register-shutdown-hook!
  "Ensure IMAP connections, the web server, and the watchdog are torn down cleanly on SIGTERM
   (e.g. `docker stop`) instead of the JVM being killed mid-flight."
  []
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread.
    ^Runnable (fn []
                (t/log! :info "Shutdown signal received. Stopping Plauna gracefully.")
                (doseq [[label teardown] [["web server" server/stop-server]
                                          ["IMAP connections" client/disconnect-all]
                                          ["watchdog" diagnostics/stop-watchdog!]]]
                  (try (teardown)
                       (catch Throwable e
                         (t/log! {:level :error :error e} ["Error while stopping" label]))))))))

(defn -main
  [& args]
  (setup-logging)
  (register-shutdown-hook!)
  (let [application-config (files/parse-config-from-cli-arguments args)
        context {:config application-config :client (ImapClient.) :db (SqliteDB.) :analyzer (BasicAnalyzer.)}]
    (let [db-config (db-cfg/load-config)]
      (db/setup-db! db-config)
      (when (= :sqlite (:type db-config))
        (files/check-and-create-database-file)))
    (db/create-db)
    (let [db-vals (into {} (map #(vector % (db/fetch-preference %))
                               [:log-level :language-detection-threshold
                                :categorization-threshold :client-health-check-interval
                                :categorization-algorithm]))]
      (when (settings/migrate-from-db-values! db-vals)
        (t/log! :info "Preferences migrated to settings.json.")))
    (auth/initialize!)
    (t/log! :info "Setting log level according to preferences.")
    (t/set-min-level! (preferences/log-level))
    (diagnostics/start-watchdog! 60)
    (start-imap-client context)
    (events/start-event-loops event-register)
    (server/start-server context)))

(comment
  (server/start-server {:config {:server {:port 8080}}})
  (server/stop-server)
  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (client/disconnect-all))
