(ns plauna.database
  (:require [clojure.java.io]
            [clojure.walk :refer [postwalk]]
            [plauna.files :as files]
            [plauna.util.async :as async-utils]
            [plauna.core.email :as core.email]
            [clojure.string :as string]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :refer [as-unqualified-lower-maps as-unqualified-kebab-maps]]
            [honey.sql :as honey]
            [honey.sql.helpers :refer [insert-into upsert values on-conflict do-update-set]]
            [next.jdbc.sql.builder :as builder]
            [plauna.util.page :as page]
            [taoensso.telemere :as t]
            [plauna.interfaces :as int]
            [clojure.core.async :as async])
  (:import (org.flywaydb.core Flyway)
           (com.zaxxer.hikari HikariDataSource HikariConfig)))

(set! *warn-on-reflection* true)

;; ── Datasource management ──────────────────────────────────────────────────────

(defonce ^:private active-db-type (atom :sqlite))

(defonce ^:private mariadb-pool (atom nil))

(defn sqlite-url []
  (str "jdbc:sqlite:" (files/path-to-db-file)
       "?busy_timeout=30000&journal_mode=WAL&synchronous=NORMAL"))

(defn setup-db!
  "Initialise the datasource for the given db-config map.
   Must be called once at startup before any other DB function.
   Falls back to SQLite if MariaDB connection fails."
  [{:keys [type host port name user password] :as cfg}]
  (reset! active-db-type type)
  (when (= :mariadb type)
    (try
      (let [hcfg (HikariConfig.)]
        (.setJdbcUrl hcfg (str "jdbc:mariadb://" host ":" port "/" name))
        (.setUsername hcfg user)
        (.setPassword hcfg (or password ""))
        (.setMaximumPoolSize hcfg 10)
        (.setMinimumIdle hcfg 2)
        (.setConnectionTimeout hcfg 30000)
        (.setValidationTimeout hcfg 5000)
        (reset! mariadb-pool (HikariDataSource. hcfg))
        (t/log! :info ["Connected to MariaDB at" (str host ":" port "/" name)]))
      (catch Exception e
        (t/log! :error ["Failed to initialise MariaDB connection pool, falling back to SQLite:" (.getMessage e)])
        (reset! active-db-type :sqlite)
        (reset! mariadb-pool nil))))
  cfg)

(defn ds
  "Return the active datasource (HikariCP pool for MariaDB, JDBC URL for SQLite)."
  []
  (or @mariadb-pool
      (jdbc/get-datasource {:jdbcUrl (sqlite-url)})))

(defn db-type [] @active-db-type)

(defn flyway []
  (let [location (if (= :mariadb @active-db-type)
                   "classpath:db/migration/mariadb"
                   "classpath:db/migration/sqlite")]
    (.load (doto (Flyway/configure)
             (.dataSource (ds))
             (.locations ^"[Ljava.lang.String;" (into-array String [location]))))))

(def my-addresses (atom #{}))

(defn create-db []
  (.migrate ^Flyway (flyway))
  (when (= :sqlite @active-db-type)
    (jdbc/execute! (ds) ["PRAGMA foreign_keys = ON;"])
    (jdbc/execute! (ds) ["PRAGMA journal_mode = WAL;"])
    (jdbc/execute! (ds) ["PRAGMA foreign_keys=on;"])))

(def builder-function {:builder-fn as-unqualified-lower-maps})

(def builder-function-kebab {:builder-fn as-unqualified-kebab-maps})

;; ── Dialect helpers ────────────────────────────────────────────────────────────

(defn- mariadb? [] (= :mariadb @active-db-type))

(defn- sql-now
  "Current time as a Unix epoch integer, dialect-appropriate."
  []
  (if (mariadb?) [:unix_timestamp] [:strftime "%s" "now"]))

;; Insert Clauses

(defn insert->insert-ignore [insert-query]
  (let [sql (first insert-query)]
    (if (mariadb?)
      ;; ON DUPLICATE KEY UPDATE col = col is a true no-op: row is unchanged, no other
      ;; constraints are bypassed, and non-duplicate errors still propagate.
      (let [first-col (second (re-find #"(?i)INSERT INTO \w+ \((\w+)" sql))]
        (conj (rest insert-query)
              (str sql " ON DUPLICATE KEY UPDATE " first-col " = " first-col)))
      (conj (rest insert-query) (string/replace sql #"INSERT" "INSERT OR IGNORE")))))

(defn insert->insert-update [insert-query]
  ;; Only used for preferences on SQLite; MariaDB callers use ON DUPLICATE KEY UPDATE directly.
  (let [insert-part (first insert-query)]
    (conj (rest insert-query) (string/replace insert-part #"INSERT" "INSERT OR REPLACE"))))

(defn insert->metadata-upsert
  "SQLite upsert for metadata that updates ONLY the supplied columns on conflict, preserving the rest of
   the row (e.g. folder, category_modified, language_modified). INSERT OR REPLACE must NOT be used here:
   it deletes and re-inserts the row, wiping any columns not present in the INSERT."
  [insert-query]
  (conj (rest insert-query)
        (str (first insert-query)
             " ON CONFLICT(message_id) DO UPDATE SET"
             " language = excluded.language,"
             " language_confidence = excluded.language_confidence,"
             " category = excluded.category,"
             " category_confidence = excluded.category_confidence")))

(defn save-headers [headers]
  (jdbc/execute! (ds)
                 (->>
                  (builder/for-insert-multi
                   :headers
                   [:mime_type :subject :message_id :date :in_reply_to]
                   (mapv (juxt :mime-type :subject :message-id :date :in-reply-to) headers) {})
                  (insert->insert-ignore))
                 {:batch true}))

(defn save-bodies [bodies]
  (jdbc/execute! (ds)
                 (->>
                  (builder/for-insert-multi
                   :bodies
                   [:content :mime_type :charset :transfer_encoding :message_id :filename :content_disposition]
                   (mapv (juxt :content :mime-type :charset :transfer-encoding :message-id :filename :content-disposition) bodies) {})
                  (insert->insert-ignore))
                 {:batch true}))

(defn save-contacts [contacts]
  (jdbc/execute! (ds)
                 (->>
                  (builder/for-insert-multi
                   :contacts
                   [:contact_key :name :address]
                   (mapv (juxt :contact-key :name :address) contacts) {})
                  (insert->insert-ignore))
                 {:batch true}))

(defn save-communications [contacts]
  (jdbc/execute! (ds)
                 (->> (builder/for-insert-multi
                       :communications
                       [:message_id :contact_key :type]
                       (mapv (juxt :message-id :contact-key (comp name :type)) contacts) {})
                      (insert->insert-ignore))
                 {:batch true}))

(defn update-metadata-batch [metadata]
  (when (seq metadata)
    (if (mariadb?)
      (doseq [m metadata]
        (jdbc/execute! (ds)
          ["INSERT INTO metadata (message_id, language, language_confidence, category, category_confidence) VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE language = VALUES(language), language_confidence = VALUES(language_confidence), category = VALUES(category), category_confidence = VALUES(category_confidence)"
           (:message-id m) (:language m) (:language-confidence m) (:category-id m) (:category-confidence m)]))
      (jdbc/execute! (ds)
                     (->> (builder/for-insert-multi
                           :metadata
                           [:message_id :language :language_confidence :category :category_confidence]
                           (mapv (juxt :message-id :language :language-confidence :category-id :category-confidence) metadata) {})
                          (insert->metadata-upsert))
                     {:batch true}))))

(def batch-size 500)

(defn empty-buffer [] {:headers [] :bodies [] :participants [] :metadata []})

(defn add-to-buffer [e-mail buffer]
  (let [updated-buffer
        (-> (update buffer :headers conj (:header e-mail))
            (update :bodies concat (:body e-mail))
            (update :participants concat (:participants e-mail)))]
    (if (some? (:metadata e-mail))
      (update updated-buffer :metadata conj (:metadata e-mail))
      updated-buffer)))

(defn- bodyless-message-ids [headers bodies]
  (let [covered (set (map :message-id bodies))]
    (vec (remove covered (map :message-id headers)))))

(defn save-emails-in-buffer [buffer]
  (try
    (save-headers (:headers buffer))
    (let [missing (bodyless-message-ids (:headers buffer) (:bodies buffer))]
      (when (seq missing)
        (t/log! :warn ["No body parts parsed for message ID(s):" missing])))
    (when (seq (:bodies buffer)) (save-bodies (:bodies buffer)))
    (when (seq (:participants buffer))
      (save-contacts (:participants buffer))
      (save-communications (:participants buffer)))
    (when (seq (:metadata buffer)) (update-metadata-batch (:metadata buffer)))
    (catch Exception e (t/log! {:level :error :error e} (.getMessage e)))))

(defn database-event-loop [publisher]
  (let [parsed-chan (async/chan)
        enriched-chan (async/chan)
        local-chan (async/merge [parsed-chan enriched-chan] batch-size)]
    (async/sub publisher :parsed-email local-chan)
    (async/sub publisher :enriched-email local-chan)
    (async/go-loop [event (async/<! local-chan)
                    buffer (empty-buffer)]
      (when (some? event)
        (cond
          (= :timed-out event)
          (do (t/log! :debug ["Received timeout. Saving everything in the buffer."])
              (save-emails-in-buffer buffer)
              (recur (async/<! local-chan) (empty-buffer)))

          (> (count (:headers buffer)) batch-size)
          (do (t/log! :debug ["DB buffer full. Emptying"])
              (let [updated-buffer (add-to-buffer (:payload event) buffer)]
                (save-emails-in-buffer updated-buffer))
              (recur (async/<! local-chan) (empty-buffer)))

          :else
          (recur (async-utils/fetch-or-timeout! local-chan 1000) (add-to-buffer (:payload event) buffer)))))))

(defn honey-intervals []
  (if (mariadb?)
    {:yearly  [:year [:from_unixtime :date]]
     :monthly [:date_format [:from_unixtime :date] "%Y-%m"]}
    {:yearly  [:strftime "%Y" [:datetime :date "unixepoch"]]
     :monthly [:strftime "%Y-%m" [:datetime :date "unixepoch"]]}))

(defn update-metadata [message_id category cat-confidence language lang-confidence]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO metadata (message_id, category, category_modified, category_confidence, language, language_modified, language_confidence) VALUES (?, ?, UNIX_TIMESTAMP(), ?, ?, UNIX_TIMESTAMP(), ?) ON DUPLICATE KEY UPDATE category = VALUES(category), category_modified = VALUES(category_modified), category_confidence = VALUES(category_confidence), language = VALUES(language), language_modified = VALUES(language_modified), language_confidence = VALUES(language_confidence)"
       message_id category cat-confidence language lang-confidence])
    (jdbc/execute! (ds) (-> (insert-into :metadata)
                            (values [{:message_id          message_id
                                      :category            category
                                      :category_modified   (sql-now)
                                      :category_confidence cat-confidence
                                      :language            language
                                      :language_modified   (sql-now)
                                      :language_confidence lang-confidence}])
                            (upsert (-> (on-conflict :message_id)
                                        (do-update-set :category
                                                       :category_modified
                                                       :category_confidence
                                                       :language
                                                       :language_modified
                                                       :language_confidence)))
                            (honey/format)))))

(defn update-metadata-category [message_id category confidence]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO metadata (message_id, category, category_modified, category_confidence) VALUES (?, ?, UNIX_TIMESTAMP(), ?) ON DUPLICATE KEY UPDATE category = VALUES(category), category_modified = VALUES(category_modified), category_confidence = VALUES(category_confidence)"
       message_id category confidence])
    (jdbc/execute! (ds) (-> (insert-into :metadata)
                            (values [{:message_id message_id :category category :category_modified (sql-now) :category_confidence confidence}])
                            (upsert (-> (on-conflict :message_id)
                                        (do-update-set :category :category_modified :category_confidence)))
                            (honey/format)))))

(defn update-metadata-language [message_id language confidence]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO metadata (message_id, language, language_modified, language_confidence) VALUES (?, ?, UNIX_TIMESTAMP(), ?) ON DUPLICATE KEY UPDATE language = VALUES(language), language_modified = VALUES(language_modified), language_confidence = VALUES(language_confidence)"
       message_id language confidence])
    (jdbc/execute! (ds) (-> (insert-into :metadata)
                            (values [{:message_id message_id :language language :language_modified (sql-now) :language_confidence confidence}])
                            (upsert (-> (on-conflict :message_id)
                                        (do-update-set :language :language_modified :language_confidence)))
                            (honey/format)))))

(defn update-email-folder
  "Record the IMAP folder a message currently lives in, so later moves can find it regardless of category-to-folder mapping changes."
  [message-id folder]
  (jdbc/execute! (ds) (honey/format {:update :metadata
                                     :set    {:folder folder}
                                     :where  [:= :message-id message-id]})))

(defn email-folder
  "Return the recorded IMAP folder for a message, or nil if none was recorded."
  [message-id]
  (:folder (jdbc/execute-one! (ds) (honey/format {:select [:folder]
                                                  :from   :metadata
                                                  :where  [:= :message-id message-id]}) builder-function)))

(defn get-categories []
  (jdbc/execute! (ds) (honey/format {:select [:*] :from :categories}) builder-function))

(defn create-category
  ([category] (create-category category nil))
  ([category destination-folder]
   (jdbc/execute! (ds) (honey/format {:insert-into :categories :columns [:name :destination_folder] :values [[category destination-folder]]}))))

(defn delete-category-by-id [id]
  (jdbc/execute! (ds) (honey/format {:delete-from :categories :where [:= :id id]})))

(defn update-category-destination-folder [id destination-folder]
  (jdbc/execute! (ds) (honey/format {:update :categories
                                     :set    {:destination_folder destination-folder}
                                     :where  [:= :id id]})))

(defn delete-email-by-message-id [message-id]
  (if (mariadb?)
    (jdbc/execute! (ds) ["DELETE FROM headers WHERE message_id = ?" message-id])
    (with-open [conn (jdbc/get-connection (ds))]
      (jdbc/execute! conn ["PRAGMA foreign_keys = ON"])
      (jdbc/execute! conn ["DELETE FROM headers WHERE message_id = ?" message-id]))))

(defn category-by-name [category-name]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from :categories :where [:= :name category-name]}) builder-function))

(defn category-by-id [id]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from :categories :where [:= :id id]}) builder-function))

(defn get-languages []
  (jdbc/execute! (ds) ["select language from metadata group by language"] builder-function))

(defn get-language-preferences []
  (jdbc/execute! (ds) ["select * from category_training_preferences where language <> 'n/a'"] builder-function))

(defn get-activated-language-preferences []
  (jdbc/execute! (ds) ["select * from category_training_preferences where use_in_training = 1"] builder-function))

(defn add-language-preferences [preferences]
  (jdbc/execute! (ds) (insert->insert-ignore (honey/format {:insert-into :category-training-preferences :columns [:language :use-in-training] :values preferences}))))

(defn update-language-preference [preference]
  (jdbc/execute! (ds) ["UPDATE category_training_preferences SET use_in_training = ?  WHERE id = ?" (:use preference) (:id preference)] builder-function))

;;;;;;;;;;;;;; Refactored call stuff

(defn headers-for-strict-options [strict]
  (if strict
    "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id"
    "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers LEFT JOIN metadata ON headers.message_id = metadata.message_id"))

(defn body-parts-for-options [] "SELECT * FROM bodies INNER JOIN metadata ON metadata.message_id = bodies.message_id")

(defn interval-for-honey [key] (get (honey-intervals) key :yearly))

(defn convert-to-count [sql-result entity]
  (let [sql (first sql-result)
        to-format (string/replace (string/replace sql #"SELECT .* FROM" "SELECT COUNT(%s) as count FROM") #"ORDER.*$" "")]
    (cond (= entity :enriched-email) (flatten [(format to-format "headers.message_id") (rest sql-result)])
          (= entity :body-part) (flatten [(format to-format "bodies.message_id") (rest sql-result)]))))

(def key-lookup {:message-id :headers.message_id
                 :date :headers.date
                 :category :metadata.category
                 :language :metadata.language
                 :category-modified :metadata.category_modified})

(defn change-important-keys [key]
  (let [lookup (get key-lookup key)]
    (if (nil? lookup)
      key
      lookup)))

(defmulti data->sql :entity)

(defmethod data->sql :body-part [_ sql-clause]
  (let [jdbc-sql (honey/format (postwalk change-important-keys sql-clause))]
    (flatten [(str (body-parts-for-options) " " (first jdbc-sql)) (rest jdbc-sql)])))

(defmethod data->sql :enriched-email
  ([entity-clause sql-clause]
   (let [strict (:strict entity-clause)
         jdbc-sql (honey/format (postwalk change-important-keys sql-clause))]
     (flatten [(str (headers-for-strict-options strict) " " (first jdbc-sql)) (rest jdbc-sql)])))
  ([entity-clause]
   (let [strict (:strict entity-clause)]
     [(headers-for-strict-options strict)])))

(defmethod data->sql :participant [_ sql-clause]
  (let [first-part  {:select [:communications.contact-key :message-id :type :name :address] :from [:communications] :join [:contacts [:= :contacts.contact-key :communications.contact-key]]}]
    (->> (conj first-part sql-clause)
         honey/format)))

(defn fetch-headers [entity-clause sql-clause] (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab))

(defn fetch-metadata [message-id] (jdbc/execute-one! (ds) ["SELECT message_id, language, language_modified, language_confidence, metadata.category AS category_id, category_modified, category_confidence, categories.name AS category FROM metadata LEFT JOIN categories ON metadata.category = categories.id WHERE metadata.message_id = ?" message-id] builder-function-kebab))

(defn fetch-bodies [message-id] (jdbc/execute! (ds) ["SELECT * FROM bodies WHERE message_id = ?" message-id] builder-function-kebab))

(defn fetch-participants [message-id] (jdbc/execute! (ds) ["SELECT * FROM communications LEFT JOIN contacts ON contacts.contact_key = communications.contact_key WHERE message_id = ? " message-id] builder-function-kebab))

(defn db->metadata [db-metadata] (apply core.email/->Metadata ((juxt :message-id :language :language-modified :language-confidence :category :category-id :category-modified :category-confidence) db-metadata)))

(defn related-data-to-header [header]
  (let [message-id (:message-id header)
        metadata (db->metadata (fetch-metadata message-id))
        bodies (map core.email/construct-body-part (fetch-bodies message-id))
        participants (map core.email/construct-participants (fetch-participants message-id))]
    (core.email/->EnrichedEmail header bodies participants metadata)))

(defn- in-clause-placeholders [n] (string/join ", " (repeat n "?")))

(defn fetch-metadata-for [message-ids]
  (when (seq message-ids)
    (jdbc/execute! (ds)
                   (into [(str "SELECT message_id, language, language_modified, language_confidence, metadata.category AS category_id, category_modified, category_confidence, categories.name AS category FROM metadata LEFT JOIN categories ON metadata.category = categories.id WHERE metadata.message_id IN (" (in-clause-placeholders (count message-ids)) ")")]
                         message-ids)
                   builder-function-kebab)))

(defn fetch-bodies-for [message-ids]
  (when (seq message-ids)
    (jdbc/execute! (ds)
                   (into [(str "SELECT * FROM bodies WHERE message_id IN (" (in-clause-placeholders (count message-ids)) ")")]
                         message-ids)
                   builder-function-kebab)))

(defn fetch-participants-for [message-ids]
  (when (seq message-ids)
    (jdbc/execute! (ds)
                   (into [(str "SELECT * FROM communications LEFT JOIN contacts ON contacts.contact_key = communications.contact_key WHERE message_id IN (" (in-clause-placeholders (count message-ids)) ")")]
                         message-ids)
                   builder-function-kebab)))

(defn related-data-to-headers
  "Batch variant of related-data-to-header. Loads metadata, bodies and participants for a whole page of
   headers in three queries total (instead of three per header), then assembles the EnrichedEmails.
   Preserves the order of the incoming headers."
  [headers]
  (let [message-ids        (mapv :message-id headers)
        metadata-by-id     (into {} (map (juxt :message-id identity)) (fetch-metadata-for message-ids))
        bodies-by-id       (group-by :message-id (fetch-bodies-for message-ids))
        participants-by-id (group-by :message-id (fetch-participants-for message-ids))]
    (mapv (fn [header]
            (let [mid (:message-id header)]
              (core.email/->EnrichedEmail
               header
               (map core.email/construct-body-part (get bodies-by-id mid))
               (map core.email/construct-participants (get participants-by-id mid))
               (db->metadata (get metadata-by-id mid)))))
          headers)))

(defmulti fetch-data (fn [options _] (:entity options)))

(defmethod fetch-data :body-part [entity-clause sql-clause]
  (if (nil? (:page entity-clause))
    (let [result (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab)]
      (map (fn [el] (core.email/->EnrichedBodyPart (core.email/construct-body-part el) (core.email/map->Metadata el))) result))
    (let [limit-offset (page/page-request->limit-offset (:page entity-clause))
          sql-clause-with-limit-offset (conj sql-clause limit-offset)
          result (jdbc/execute! (ds) (data->sql entity-clause sql-clause-with-limit-offset) builder-function-kebab)
          data (map (fn [el] (core.email/->EnrichedBodyPart (core.email/construct-body-part el) (core.email/map->Metadata el))) result)]
      {:data  data
       :size  (count data)
       :page  (inc (quot (:offset limit-offset) (:limit limit-offset)))
       :total (:count (jdbc/execute-one! (ds) (convert-to-count (data->sql entity-clause sql-clause) (:entity entity-clause)) builder-function-kebab))})))

(defmethod fetch-data :enriched-email [entity-clause sql-clause]
  (if (nil? (:page entity-clause))
    (related-data-to-headers (map core.email/construct-header (fetch-headers entity-clause sql-clause)))
    (let [limit-offset (page/page-request->limit-offset (:page entity-clause))
          sql-clause-with-limit-offset (conj sql-clause limit-offset)
          data (related-data-to-headers (map core.email/construct-header (fetch-headers entity-clause sql-clause-with-limit-offset)))]
      {:data  data
       :size  (count data)
       :page  (inc (quot (:offset limit-offset) (:limit limit-offset)))
       :total (:count (jdbc/execute-one! (ds) (convert-to-count (data->sql entity-clause sql-clause) (:entity entity-clause)) builder-function-kebab))})))

(defmethod fetch-data :participant [entity-clause sql-clause]
  (map core.email/map->Participant (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab)))

(defn yearly-email-stats []
  (jdbc/execute! (ds) ["SELECT COUNT(message_id) AS count, date AS date FROM headers WHERE date IS NOT NULL GROUP BY date"] builder-function-kebab))

(defn query-db [honeysql-query]
  (jdbc/execute! (ds) (honey/format honeysql-query) builder-function-kebab))

(comment (honey/format
          {:select [[[:count :headers.message-id] :count] :bodies.mime-type] :from [:bodies]
           :join [:headers [:= :bodies.message-id :headers.message_id]]
           :group-by [:bodies.mime-type]
           :order-by [[:count :desc]]}))

(defn update-preference [preference value]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO preferences (preference, value) VALUES (?, ?) ON DUPLICATE KEY UPDATE value = VALUES(value)"
       (name preference) value])
    (jdbc/execute! (ds)
                   (-> {:insert-into [:preferences]
                        :columns     [:preference :value]
                        :values      [[(name preference) value]]}
                       (honey/format)
                       (insert->insert-update)) builder-function-kebab)))

(defn fetch-preference [preference]
  (let [result (jdbc/execute-one! (ds)
                                  (honey/format {:select [:value]
                                                 :from [:preferences]
                                                 :where [:= :preference (name preference)]}) builder-function-kebab)]
    (when (some? result) (:value result))))

(defn- coerce-bool
  "SQLite stores BOOLEANs as 1/0 integers; MariaDB JDBC can return Java true/false.
   Normalise both representations to a Clojure boolean."
  [v]
  (or (= 1 v) (true? v)))

(defn db-connection->model [db-conn]
  (apply (comp core.email/map->ImapConnection
               (fn [conn] (update conn :check-ssl-certs coerce-bool))
               (fn [conn] (update conn :debug coerce-bool))) [db-conn]))

(defn get-connections [] (map
                          db-connection->model
                          (jdbc/execute! (ds) (honey/format {:select [:*] :from [:connections]}) builder-function-kebab)))

(defn get-connection [id] (db-connection->model (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:connections] :where [:= :id id]}) builder-function-kebab)))

(defn get-oauth-tokens [connection-id] (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:oauth-tokens] :where [:= :connection-id connection-id]}) builder-function-kebab))

(defn save-oauth-token [token-response]
  (jdbc/execute! (ds)
                 (-> {:insert-into [:oauth_tokens]
                      :columns     [:access_token :connection-id :expires_in :refresh_token :scope :token_type]
                      :values      [[(:access_token token-response) (:connection-id token-response) (:expires_in token-response) (:refresh_token token-response) (:scope token-response) (:token_type token-response)]]}
                     (honey/format))))

(defn update-access-token [connection-id token-response]
  (if (nil? (:refresh_token token-response))
    (jdbc/execute! (ds) (-> {:update [:oauth_tokens]
                             :set {:access_token (:access_token token-response) :expires_in (:expires_in token-response)}
                             :where [:= :connection_id connection-id]}
                            (honey/format)))
    (jdbc/execute! (ds) (-> {:update [:oauth_tokens]
                             :set {:access_token (:access_token token-response) :expires_in (:expires_in token-response) :refresh_token (:refresh_token token-response)}
                             :where [:= :connection_id connection-id]}
                            (honey/format)))))

(defn delete-access-token [connection-id]
  (jdbc/execute! (ds) (honey/format {:delete-from [:oauth_tokens]
                                     :where [:= :connection_id connection-id]}) builder-function-kebab))

(defn add-connection [connection]
  (jdbc/execute! (ds)
                 (honey/format {:insert-into [:connections]
                                :columns [:id :host :user :secret :folder :debug :security :port :check-ssl-certs]
                                :values [[(:id connection) (:host connection) (:user connection)
                                          (:secret connection) (:folder connection) (:debug connection) (:security connection) (:port connection) (:check-ssl-certs connection)]]})
                 builder-function))

(defn update-connection [connection]
  (jdbc/execute! (ds)
                 (honey/format {:update [:connections]
                                :set {:host (:host connection) :user (:user connection) :secret (:secret connection) :folder (:folder connection) :debug (:debug connection) :port (:port connection) :security (:security connection) :check-ssl-certs (:check-ssl-certs connection) :auth_type (:auth-type connection) :auth-provider (:auth-provider connection)}
                                :where  [:= :id (:id connection)]})
                 builder-function))

(defn delete-connection [id] (jdbc/execute! (ds) (honey/format {:delete-from [:connections]
                                                                :where [:= :id id]}) builder-function-kebab))

(defn add-auth-provider [provider]
  (jdbc/execute! (ds)
                 (honey/format {:insert-into [:auth_providers]
                                :values [provider]})
                 builder-function))

(defn get-auth-providers []
  (jdbc/execute! (ds) (honey/format {:select [:*] :from [:auth_providers]}) builder-function-kebab))

(defn get-auth-provider [id]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:auth_providers] :where [:= :id id]}) builder-function-kebab))

(defn delete-auth-provider [id] (jdbc/execute! (ds) (honey/format {:delete-from [:auth_providers]
                                                                   :where [:= :id id]}) builder-function-kebab))

(defn update-auth-provider [provider]
  (let [wo-id (dissoc provider :id)]
    (jdbc/execute! (ds)
                   (honey/format {:update [:auth_providers]
                                  :set wo-id
                                  :where  [:= :id (:id provider)]})
                   builder-function)))

(defn close-pool!
  "Shut down the MariaDB connection pool gracefully (called on app stop)."
  []
  (when-let [pool ^HikariDataSource @mariadb-pool]
    (.close pool)
    (reset! mariadb-pool nil)))

(deftype SqliteDB []
  int/DB
  (fetch-connection [_ id] (get-connection id))
  (fetch-oauth-token-data [_ connection-id] (get-oauth-tokens connection-id))
  (fetch-auth-provider [_ id] (get-auth-provider id))
  (fetch-categories [_] (get-categories))
  (fetch-emails [_ entity customization] (fetch-data entity customization))
  (save-category [_ category-name destination-folder] (create-category category-name destination-folder))
  (update-category-destination-folder [_ id destination-folder] (update-category-destination-folder id destination-folder))
  (update-email-folder [_ message-id folder] (update-email-folder message-id folder))
  (save-email [_ email]
    (save-headers [(:header email)])
    (if (seq (:body email))
      (save-bodies (:body email))
      (t/log! :warn ["No body parts parsed for message ID:" (-> email :header :message-id)]))
    (when (seq (:participants email))
      (save-contacts (:participants email))
      (save-communications (:participants email)))
    (when (seq (:metadata email)) (update-metadata-batch [(:metadata email)]))))
