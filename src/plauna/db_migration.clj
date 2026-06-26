(ns plauna.db-migration
  "One-shot data migration from a SQLite file to a configured MariaDB database.
   Reads from SQLite directly (regardless of the current active datasource), runs
   Flyway on the target MariaDB to create the schema, then copies every table in
   FK-safe order using ON DUPLICATE KEY UPDATE col = col so partial re-runs are idempotent."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :refer [as-unqualified-lower-maps]]
            [plauna.database :as db]
            [plauna.db-config :as db-cfg]
            [plauna.files :as files]
            [taoensso.telemere :as t])
  (:import (org.flywaydb.core Flyway)
           (com.zaxxer.hikari HikariDataSource HikariConfig)))

(set! *warn-on-reflection* true)

;; Tables in insertion order — parents before children.
(def ^:private migration-order
  ["preferences" "contacts" "categories" "category_training_preferences"
   "auth_providers" "headers" "connections" "bodies" "communications"
   "metadata" "oauth_tokens"])

(defn- sqlite-ds []
  (jdbc/get-datasource
   {:jdbcUrl (str "jdbc:sqlite:" (files/path-to-db-file)
                  "?busy_timeout=10000&journal_mode=WAL")}))

(defn- mariadb-ds [{:keys [host port name user password]}]
  (let [hcfg (HikariConfig.)]
    (.setJdbcUrl hcfg (str "jdbc:mariadb://" host ":" port "/" name))
    (.setUsername hcfg user)
    (.setPassword hcfg (or password ""))
    (.setMaximumPoolSize hcfg 5)
    (.setConnectionTimeout hcfg 15000)
    (HikariDataSource. hcfg)))

(defn- run-flyway! [^HikariDataSource pool]
  (.migrate
   (.load (doto (Flyway/configure)
            (.dataSource pool)
            (.locations ^"[Ljava.lang.String;"
                        (into-array String ["classpath:db/migration/mariadb"]))))))

(defn- migrate-table! [src-ds ^HikariDataSource dst-ds table]
  (let [rows     (jdbc/execute! src-ds [(str "SELECT * FROM " table)]
                                {:builder-fn as-unqualified-lower-maps})
        n        (count rows)
        inserted (atom 0)
        skipped  (atom 0)]
    (t/log! :info ["Migrating" table (str "(" n " rows)")])
    (when (pos? n)
      (doseq [row rows]
        (let [cols      (map name (keys row))
              vals      (vec (vals row))
              first-col (first cols)
              ;; col = col is a true no-op on duplicate: row is not mutated, but non-duplicate
              ;; errors (type mismatch, FK violation) still propagate so integrity is enforced.
              sql  (str "INSERT INTO " table
                        " (" (clojure.string/join ", " cols) ")"
                        " VALUES (" (clojure.string/join ", " (repeat (count cols) "?")) ")"
                        " ON DUPLICATE KEY UPDATE " first-col " = " first-col)]
          (try
            (let [result (jdbc/execute! dst-ds (into [sql] vals))
                  cnt    (get (first result) :next.jdbc/update-count 0)]
              (if (pos? cnt)
                (swap! inserted inc)
                (swap! skipped inc)))
            (catch Exception e
              (swap! skipped inc)
              (t/log! :warn ["Failed row in" table ":" (.getMessage e)]))))))
    (when (pos? @skipped)
      (t/log! :warn ["Table" table ":" @skipped "of" n "rows were skipped (duplicates or errors). Inserted:" @inserted]))
    {:total n :inserted @inserted :skipped @skipped}))

(defn migrate!
  "Run the full SQLite → MariaDB migration.
   Returns {:ok true :counts {table n ...}} or {:ok false :error message}."
  []
  (let [cfg (db-cfg/load-config)]
    (if (not= :mariadb (:type cfg))
      {:ok false :error "No MariaDB configuration found. Save a MariaDB config first."}
      (let [src (sqlite-ds)]
        (try
          (t/log! :info "Starting SQLite → MariaDB migration.")
          (with-open [dst (mariadb-ds cfg)]
            (t/log! :info "Running Flyway on MariaDB target.")
            (run-flyway! dst)
            (let [counts       (into {} (for [t migration-order]
                                          [t (migrate-table! src dst t)]))
                  total-skip   (reduce + (map #(:skipped (val %)) counts))]
              (t/log! :info ["Migration complete. Total skipped rows:" total-skip "Counts:" counts])
              {:ok true :counts counts :skipped-total total-skip}))
          (catch Exception e
            (t/log! :error ["Migration failed:" (.getMessage e)])
            {:ok false :error (.getMessage e)}))))))

(defn test-connection!
  "Try to open one connection to the configured MariaDB. Returns {:ok true} or {:ok false :error message}."
  [{:keys [host port name user password]}]
  (try
    (with-open [ds (mariadb-ds {:host host :port port :name name :user user :password password})]
      (jdbc/execute-one! ds ["SELECT 1"])
      {:ok true})
    (catch Exception e
      {:ok false :error (.getMessage e)})))
