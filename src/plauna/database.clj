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
  ;; foreign_keys is a connection-scoped pragma, and every jdbc/execute! may open a fresh
  ;; connection — putting it in the URL is the only way it reliably applies everywhere.
  (str "jdbc:sqlite:" (files/path-to-db-file)
       "?busy_timeout=30000&journal_mode=WAL&synchronous=NORMAL&foreign_keys=true"))

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
  ;; SQLite pragmas (foreign_keys, WAL, busy_timeout) are set in the JDBC URL — see sqlite-url —
  ;; because they are connection-scoped and would not carry over to later connections if set here.
  (.migrate ^Flyway (flyway)))

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

(defn email-exists? [message-id]
  (some? (jdbc/execute-one! (ds)
           (honey/format {:select [1] :from :headers :where [:= :message_id message-id]})
           builder-function)))

(defn save-headers
  ([headers] (save-headers (ds) headers))
  ([conn headers]
   (jdbc/execute! conn
                  (->>
                   (builder/for-insert-multi
                    :headers
                    [:mime_type :subject :message_id :date :in_reply_to]
                    (mapv (juxt :mime-type :subject :message-id :date :in-reply-to) headers) {})
                   (insert->insert-ignore))
                  {:batch true})))

(defn save-bodies
  ([bodies] (save-bodies (ds) bodies))
  ([conn bodies]
   (jdbc/execute! conn
                  (->>
                   (builder/for-insert-multi
                    :bodies
                    [:content :mime_type :charset :transfer_encoding :message_id :filename :content_disposition]
                    (mapv (juxt :content :mime-type :charset :transfer-encoding :message-id :filename :content-disposition) bodies) {})
                   (insert->insert-ignore))
                  {:batch true})))

(defn delete-bodies-by-ids
  "Delete the body rows with the given primary-key ids. Used by refetch to drop a specific stale (empty)
   body row before saving its repaired, content-bearing version. Deleting by id keeps the match exact
   (and dialect-agnostic) instead of relying on null-safe multi-column SQL."
  [ids]
  (when (seq ids)
    (jdbc/execute! (ds)
                   (into [(str "DELETE FROM bodies WHERE id IN (" (string/join ", " (repeat (count ids) "?")) ")")]
                         ids))))

(defn save-contacts
  ([contacts] (save-contacts (ds) contacts))
  ([conn contacts]
   (jdbc/execute! conn
                  (->>
                   (builder/for-insert-multi
                    :contacts
                    [:contact_key :name :address]
                    (mapv (juxt :contact-key :name :address) contacts) {})
                   (insert->insert-ignore))
                  {:batch true})))

(defn save-communications
  ([contacts] (save-communications (ds) contacts))
  ([conn contacts]
   (jdbc/execute! conn
                  (->> (builder/for-insert-multi
                        :communications
                        [:message_id :contact_key :type]
                        (mapv (juxt :message-id :contact-key (comp name :type)) contacts) {})
                       (insert->insert-ignore))
                  {:batch true})))

(defn update-metadata-batch
  ([metadata] (update-metadata-batch (ds) metadata))
  ([conn metadata]
   (when (seq metadata)
     (if (mariadb?)
       (doseq [m metadata]
         (jdbc/execute! conn
           ["INSERT INTO metadata (message_id, language, language_confidence, category, category_confidence) VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE language = VALUES(language), language_confidence = VALUES(language_confidence), category = VALUES(category), category_confidence = VALUES(category_confidence)"
            (:message-id m) (:language m) (:language-confidence m) (:category-id m) (:category-confidence m)]))
       (jdbc/execute! conn
                      (->> (builder/for-insert-multi
                            :metadata
                            [:message_id :language :language_confidence :category :category_confidence]
                            (mapv (juxt :message-id :language :language-confidence :category-id :category-confidence) metadata) {})
                           (insert->metadata-upsert))
                      {:batch true})))))

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

(defn save-emails-in-buffer
  "Persist everything in the buffer in ONE transaction: either the whole batch commits or nothing
   does. Without this, a failure after save-headers would leave partial emails whose headers make
   email-exists? true, so backfills would permanently skip re-saving their missing bodies/metadata.
   A failure propagates to the caller; the rollback leaves no header behind, so the same messages
   are picked up again by a later backfill or re-parse."
  [buffer]
  (jdbc/with-transaction [tx (ds)]
    (save-headers tx (:headers buffer))
    (let [missing (bodyless-message-ids (:headers buffer) (:bodies buffer))]
      (when (seq missing)
        (t/log! :warn ["No body parts parsed for message ID(s):" missing])))
    (when (seq (:bodies buffer)) (save-bodies tx (:bodies buffer)))
    (when (seq (:participants buffer))
      (save-contacts tx (:participants buffer))
      (save-communications tx (:participants buffer)))
    (when (seq (:metadata buffer)) (update-metadata-batch tx (:metadata buffer)))))

(defn- save-buffer-logging-errors!
  "Keep the database event loop alive when a batch save fails: the transaction has already rolled
   back, so nothing partial was written and the messages remain recoverable."
  [buffer]
  (try
    (save-emails-in-buffer buffer)
    (catch Exception e
      (t/log! {:level :error :error e}
              ["Saving an email batch failed and was rolled back. The messages were not written and will be picked up by a later backfill or re-parse:" (.getMessage e)]))))

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
              (save-buffer-logging-errors! buffer)
              (recur (async/<! local-chan) (empty-buffer)))

          (> (count (:headers buffer)) batch-size)
          (do (t/log! :debug ["DB buffer full. Emptying"])
              (let [updated-buffer (add-to-buffer (:payload event) buffer)]
                (save-buffer-logging-errors! updated-buffer))
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
  ([category] (create-category category nil nil))
  ([category destination-folder] (create-category category destination-folder nil))
  ([category destination-folder color]
   (jdbc/execute! (ds) (honey/format {:insert-into :categories :columns [:name :destination_folder :color] :values [[category destination-folder color]]}))))

(defn delete-category-by-id
  "Delete a category and, in the same transaction, clear it from every metadata row that still
   references it. metadata.category is a non-cascading foreign key, so deleting first would fail
   under MariaDB (and now under SQLite too) — and skipping the cleanup would leave dangling ids."
  [id]
  (jdbc/with-transaction [tx (ds)]
    (jdbc/execute! tx ["UPDATE metadata SET category = NULL, category_confidence = NULL WHERE category = ?" id])
    (jdbc/execute! tx ["DELETE FROM categories WHERE id = ?" id])))

(defn update-category [id destination-folder color]
  (jdbc/execute! (ds) (honey/format {:update :categories
                                     :set    {:destination_folder destination-folder :color color}
                                     :where  [:= :id id]})))

(defn delete-email-by-message-id [message-id]
  ;; Cascades to bodies/communications/metadata; SQLite enforces this via foreign_keys=true in the URL.
  (jdbc/execute! (ds) ["DELETE FROM headers WHERE message_id = ?" message-id]))

(defn category-by-name [category-name]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from :categories :where [:= :name category-name]}) builder-function))

(defn category-by-id [id]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from :categories :where [:= :id id]}) builder-function))

(def distinct-value-limit
  "Cap the Subject/From/To filter dropdowns to this many distinct values. They're rendered as
   checkboxes in the page (with a client-side search box to narrow them), so an unbounded list
   could mean tens of thousands of DOM nodes on a large mailbox."
  500)

(defn distinct-subjects
  "Distinct, non-blank subjects. other-filters-where (a honeysql where-clause built from the OTHER
   active column filters, or nil) scopes the list to subjects that are still reachable given those
   filters — e.g. once a category is picked, only subjects that occur in that category are offered —
   the same way Excel's own AutoFilter narrows a column's dropdown as other filters are applied.
   The join to metadata exists only so other-filters-where can reference metadata columns."
  ([] (distinct-subjects nil))
  ([other-filters-where]
   (let [base-cond [:and [:is-not :headers.subject nil] [:<> :headers.subject ""]]
         result (jdbc/execute! (ds) (honey/format {:select-distinct [:headers.subject]
                                                    :from [:headers]
                                                    :left-join [:metadata [:= :headers.message-id :metadata.message-id]]
                                                    :where (if other-filters-where [:and base-cond other-filters-where] base-cond)
                                                    :order-by [[:headers.subject :asc]]
                                                    :limit distinct-value-limit})
                                builder-function)]
     (when (= distinct-value-limit (count result))
       (t/log! :info ["Subject filter: more than" distinct-value-limit "distinct subjects exist; showing only the first" distinct-value-limit]))
     result)))

(defn- distinct-contacts-by-type
  "Distinct (contact-key, name, address) contacts that appear with any of participant-types
   (e.g. [\"sender\" \":sender\"] — legacy rows may store the type with a leading colon, see the
   defensive strip in core.email/construct-participants). other-filters-where scopes the list to
   contacts still reachable given the OTHER active column filters — see distinct-subjects."
  ([participant-types] (distinct-contacts-by-type participant-types nil))
  ([participant-types other-filters-where]
   (let [type-cond [:in :communications.type participant-types]
         result (jdbc/execute! (ds) (honey/format {:select-distinct [:contacts.contact-key :contacts.name :contacts.address]
                                                    :from [:contacts]
                                                    :join [:communications [:= :communications.contact-key :contacts.contact-key]
                                                           :headers [:= :headers.message-id :communications.message-id]]
                                                    :left-join [:metadata [:= :headers.message-id :metadata.message-id]]
                                                    :where (if other-filters-where [:and type-cond other-filters-where] type-cond)
                                                    :order-by [[:contacts.address :asc]]
                                                    :limit distinct-value-limit})
                                builder-function)]
     (when (= distinct-value-limit (count result))
       (t/log! :info ["Contact filter: more than" distinct-value-limit "distinct contacts exist for types" participant-types "; showing only the first" distinct-value-limit]))
     result)))

(defn distinct-senders
  ([] (distinct-contacts-by-type ["sender" ":sender"]))
  ([other-filters-where] (distinct-contacts-by-type ["sender" ":sender"] other-filters-where)))

(defn distinct-recipients
  ([] (distinct-contacts-by-type ["receiver" ":receiver"]))
  ([other-filters-where] (distinct-contacts-by-type ["receiver" ":receiver"] other-filters-where)))

(defn distinct-header-categories
  "Distinct metadata.category values (a real category id, or nil for uncategorized) among headers
   matching other-filters-where (or every header, if nil). Used to scope the Category filter's
   checklist to categories still reachable given the OTHER active column filters — see
   distinct-subjects — since categories themselves come from a small, separate table rather than
   being read off the headers/metadata rows directly."
  [other-filters-where]
  (jdbc/execute! (ds) (honey/format (cond-> {:select-distinct [:metadata.category]
                                             :from [:headers]
                                             :left-join [:metadata [:= :headers.message-id :metadata.message-id]]}
                                     other-filters-where (assoc :where other-filters-where)))
                 builder-function))

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
        ;; replace-first (not replace): a WHERE ... IN (SELECT ... FROM ...) subquery (e.g. the "From"
        ;; e-mail filter) has its own "SELECT ... FROM" later in the string. A global replace would
        ;; rewrite that one too, leaving a second unfilled "%s" for the single `format` argument below.
        ;; Non-greedy ".*?" keeps this first replacement scoped to the outer projection up to its FROM;
        ;; (?is) makes it case-insensitive and dot-matches-newline. Strip a trailing ORDER BY (counts
        ;; must not carry ordering).
        to-format (-> sql
                      (string/replace-first #"(?is)SELECT .*? FROM" "SELECT COUNT(%s) as count FROM")
                      (string/replace #"(?is)\s+ORDER BY .*$" ""))]
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
   Preserves the order of the incoming headers. Pass {:with-bodies false} to skip loading body
   content (fetch-bodies-for pulls the full MIME text of every e-mail) for consumers that only
   render header-level data, like the e-mail list."
  ([headers] (related-data-to-headers headers {:with-bodies true}))
  ([headers {:keys [with-bodies]}]
   (let [message-ids        (mapv :message-id headers)
         metadata-by-id     (into {} (map (juxt :message-id identity)) (fetch-metadata-for message-ids))
         bodies-by-id       (if with-bodies (group-by :message-id (fetch-bodies-for message-ids)) {})
         participants-by-id (group-by :message-id (fetch-participants-for message-ids))]
     (mapv (fn [header]
             (let [mid (:message-id header)]
               (core.email/->EnrichedEmail
                header
                (map core.email/construct-body-part (get bodies-by-id mid))
                (map core.email/construct-participants (get participants-by-id mid))
                (db->metadata (get metadata-by-id mid)))))
           headers))))

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
  ;; :with-bodies defaults to true; only an explicit false skips body content.
  (let [related-opts {:with-bodies (not (false? (:with-bodies entity-clause)))}]
    (if (nil? (:page entity-clause))
      (related-data-to-headers (map core.email/construct-header (fetch-headers entity-clause sql-clause)) related-opts)
      (let [limit-offset (page/page-request->limit-offset (:page entity-clause))
            sql-clause-with-limit-offset (conj sql-clause limit-offset)
            data (related-data-to-headers (map core.email/construct-header (fetch-headers entity-clause sql-clause-with-limit-offset)) related-opts)]
        {:data  data
         :size  (count data)
         :page  (inc (quot (:offset limit-offset) (:limit limit-offset)))
         :total (:count (jdbc/execute-one! (ds) (convert-to-count (data->sql entity-clause sql-clause) (:entity entity-clause)) builder-function-kebab))}))))

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

(defn save-oauth-token
  "Upsert the token row for a connection (connection_id is unique, see migration V12). COALESCE
   keeps the stored refresh token when the provider omits one on re-authorization — Google and
   others only return a refresh token on the first consent."
  [token-response]
  (let [params [(:connection-id token-response) (:access_token token-response) (:expires_in token-response)
                (:refresh_token token-response) (:scope token-response) (:token_type token-response)]]
    (if (mariadb?)
      (jdbc/execute! (ds)
        (into ["INSERT INTO oauth_tokens (connection_id, access_token, expires_in, refresh_token, scope, token_type) VALUES (?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE access_token = VALUES(access_token), expires_in = VALUES(expires_in), refresh_token = COALESCE(VALUES(refresh_token), refresh_token), scope = VALUES(scope), token_type = VALUES(token_type)"] params))
      (jdbc/execute! (ds)
        (into ["INSERT INTO oauth_tokens (connection_id, access_token, expires_in, refresh_token, scope, token_type) VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT(connection_id) DO UPDATE SET access_token = excluded.access_token, expires_in = excluded.expires_in, refresh_token = COALESCE(excluded.refresh_token, oauth_tokens.refresh_token), scope = excluded.scope, token_type = excluded.token_type"] params)))))

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
                                :columns [:id :host :user :secret :folder :debug :security :port :check-ssl-certs :auth_type :auth_provider]
                                :values [[(:id connection) (:host connection) (:user connection)
                                          (:secret connection) (:folder connection) (:debug connection) (:security connection) (:port connection) (:check-ssl-certs connection)
                                          (:auth-type connection) (:auth-provider connection)]]})
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

(defn delete-auth-provider
  "Delete a provider after detaching it from any connection that references it —
   connections.auth_provider is a non-cascading foreign key."
  [id]
  (jdbc/with-transaction [tx (ds)]
    (jdbc/execute! tx ["UPDATE connections SET auth_provider = NULL WHERE auth_provider = ?" id])
    (jdbc/execute! tx ["DELETE FROM auth_providers WHERE id = ?" id])))

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
  (fetch-distinct-subjects [_ other-filters-where] (distinct-subjects other-filters-where))
  (fetch-distinct-senders [_ other-filters-where] (distinct-senders other-filters-where))
  (fetch-distinct-recipients [_ other-filters-where] (distinct-recipients other-filters-where))
  (fetch-header-categories [_ other-filters-where] (distinct-header-categories other-filters-where))
  (fetch-emails [_ entity customization] (fetch-data entity customization))
  (save-category [_ category-name destination-folder color] (create-category category-name destination-folder color))
  (update-category [_ id destination-folder color] (update-category id destination-folder color))
  (update-email-folder [_ message-id folder] (update-email-folder message-id folder))
  (email-exists? [_ message-id] (email-exists? message-id))
  (save-email [_ email]
    ;; One transaction per email, and failures propagate: a partially-saved email whose header
    ;; already exists would otherwise be skipped by every future backfill (see save-emails-in-buffer).
    (jdbc/with-transaction [tx (ds)]
      (save-headers tx [(:header email)])
      (if (seq (:body email))
        (save-bodies tx (:body email))
        (t/log! :warn ["No body parts parsed for message ID:" (-> email :header :message-id)]))
      (when (seq (:participants email))
        (save-contacts tx (:participants email))
        (save-communications tx (:participants email)))
      (when (seq (:metadata email)) (update-metadata-batch tx [(:metadata email)])))))
