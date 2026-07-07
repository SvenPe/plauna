(ns plauna.application
  (:require [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [plauna.core.email :as core-email]
            [plauna.util.page :as page]))

(defn- escape-like
  "Escape LIKE wildcards (% and _) and the escape character itself so user input matches literally.
   Must be paired with an explicit ESCAPE '\\' in the LIKE expression: SQLite has no default escape
   character, so without it '_' matches any character and '%' matches everything."
  [text]
  (str/replace text #"([\\%_])" "\\\\$1"))

(defn- like-contains [column text]
  [:like column [:escape (str "%" (escape-like text) "%") "\\"]])

(defn- filter->where [filter]
  (cond
    (= filter "enriched-only") [:and [:<> :metadata.category nil] [:<> :metadata.language nil]]
    (= filter "without-category") [:= :metadata.category nil]
    :else nil))

(defn- subject->where
  "A blank search-text adds no filter. Without this guard a nil text builds LIKE '%%',
   which silently excludes e-mails whose subject is NULL."
  [search-field search-text]
  (when (and (= search-field "subject") (not (str/blank? search-text)))
    (like-contains :headers.subject search-text)))

(defn- from->where
  "Build a where-clause matching e-mails whose sender's name or address contains search-text.
   A blank/nil search-text adds no filter. A non-correlated IN subquery (not a correlated EXISTS):
   the matching message-ids are resolved once instead of probing contacts for every header row —
   the count query re-runs the whole clause, so a correlated probe would run twice per page load.
   Legacy rows may store the participant type as \":sender\" (see the defensive strip in
   core.email/construct-participants), so match both spellings."
  [search-text]
  (when-not (str/blank? search-text)
    [:in :headers.message-id
     {:select [:communications.message-id]
      :from [:communications]
      :join [:contacts [:= :contacts.contact-key :communications.contact-key]]
      :where [:and
              [:in :communications.type ["sender" ":sender"]]
              [:or
               (like-contains :contacts.name search-text)
               (like-contains :contacts.address search-text)]]}]))

(defn- date-string->epoch-seconds
  "Convert a 'YYYY-MM-DD' string to a UTC unix timestamp (seconds) at the start of that day,
   or the start of the following day when next-day? is true (used for an inclusive upper bound)."
  [date-str next-day?]
  (let [^java.time.LocalDate base (java.time.LocalDate/parse date-str)
        ^java.time.LocalDate day (if next-day? (.plusDays base 1) base)]
    (.toEpochSecond (.atStartOfDay day java.time.ZoneOffset/UTC))))

(defn- date->where
  "Build a where-clause filtering headers.date (stored as unix seconds) between the given dates.
   Either bound may be blank/nil. The upper bound is inclusive of the whole 'to' day."
  [date-from date-to]
  (let [from (when-not (str/blank? date-from) (date-string->epoch-seconds date-from false))
        to (when-not (str/blank? date-to) (date-string->epoch-seconds date-to true))]
    (cond
      (and from to) [:and [:>= :date from] [:< :date to]]
      from [:>= :date from]
      to [:< :date to]
      :else nil)))

(defn- combine-wheres
  "AND together the active where-clauses; nil (no filter at all) when none are active."
  [wheres]
  (let [active (remove nil? wheres)]
    (cond (empty? active) nil
          (= 1 (count active)) (first active)
          :else (into [:and] active))))

(defn- success-result [result-type data] (conj {:result result-type} data))

(defn- error-result [exception alert-content] {:result :error :exception exception :message {:type :alert :content alert-content}})

(defn categories
  "There is no entry for 'no entry' in the database. This function adds a 'n/a' entry to the actual list."
  [db] (conj (int/fetch-categories db) {:id nil :name "n/a"}))

(defn connect-to-client
  "Returns {:result :ok} or {:result :redirect :provider provider} in case of oauth2"
  [{:keys [db client] :as context} id]
  (try
    (let [connection (int/fetch-connection db id)]
      (if (= "oauth2" (:auth-type connection))
        (let [auth-provider (int/fetch-auth-provider db (:auth-provider connection))
              oauth-data (int/fetch-oauth-token-data db id)]
          (cond
            (nil? auth-provider) (throw (ex-info "Auth type is 'oauth2' but there is no auth provider." {:connection connection}))
            (or (nil? oauth-data) (nil? (:access-token oauth-data)) (nil? (:refresh-token oauth-data)))
            (do
              (t/log! :warn ["Connection" (:user connection) (:host connection) "is set to use oauth2 but has no tokens in the db. You need to login manually from the 'Connections' page first."])
              (success-result :redirect {:provider (int/fetch-auth-provider db (:auth-provider connection))}))
            :else (do (int/start-monitor client connection context) (success-result :ok nil))))
        (do (int/start-monitor client connection context) {:result :ok})))
    (catch Exception e (do (t/log! :error ["There was an error when trying to log in:" e])
                           (error-result e "There was an error when trying to log in.")))))

(defn fetch-emails
  "Returns a list of emails. Customizable by parameters which can contain the following keys:
   :size, :page, :filter (all, enrieched-only, or without-category), :search-field (subject), :search-text,
   :from-search-text (matches the sender's name or address), :date-from, :date-to"
  [context parameters]
  (let [db (:db context)
        cat-list (categories db)
        where (combine-wheres [(filter->where (:filter parameters))
                               (subject->where (:search-field parameters) (:search-text parameters))
                               (from->where (:from-search-text parameters))
                               (date->where (:date-from parameters) (:date-to parameters))])
        customization-clause (cond-> {:order-by [[:date :desc]]}
                               where (assoc :where where))
        page-req (page/page-request (:page parameters) (:size parameters))
        ;; :with-bodies false — the list renders no body content, so don't materialize
        ;; up to 500 e-mails' full MIME bodies per page.
        result (int/fetch-emails db {:entity :enriched-email :strict false :page page-req :with-bodies false} customization-clause)]
    {:data (:data result)
     :parameters {:filter (:filter parameters)
                  :total-pages (page/calculate-pages-total (:total result) (:size page-req))
                  :size (:size page-req)
                  :page (:page result)
                  :total (:total result)
                  :search-text (:search-text parameters)
                  :from-search-text (:from-search-text parameters)
                  :date-from (:date-from parameters)
                  :date-to (:date-to parameters)}
     :optional {:categories cat-list}}))

(defn create-new-category! [context category destination-folder]
  (let [db (:db context)
        client (:client context)
        cleaned (when-not (str/blank? destination-folder) (str/trim destination-folder))]
    (int/save-category db category cleaned)
    (doseq [connection-data (vals (int/connections client))]
      (int/create-category-directories! client connection-data [category]))))

(defn update-category-destination-folder!
  "Persist the destination folder for a category and make sure the folder exists on every active connection.
   A blank destination-folder restores the default 'Categories/<Name>' behaviour. Existing emails are not moved."
  [context id category-name destination-folder]
  (let [db (:db context)
        client (:client context)
        cleaned (when-not (str/blank? destination-folder) (str/trim destination-folder))]
    (int/update-category-destination-folder db id cleaned)
    (doseq [connection-data (vals (int/connections client))]
      (int/create-category-directories! client connection-data [category-name]))))

(defn move-email-to-category
  "Email address of the recipient is usually the 'username' in the connection data. It may be different, if the user is using some kind of email masking service. If the email and the username match, we know where to look for. If not, we have to loop over the connections and try to find the email by id before moving it to its new directory. This all pressupposes that the message-id is really unique."
  [email category {:keys [client] :as context}]
  (let [connections (vals (int/connections client))
        connection-id-guess (int/connection-id-for-email client connections email)
        message-id (-> email :header :message-id)
        old-category (-> email :metadata :category)]
    (try
      (cond (nil? (seq connections))
            (error-result nil "There are no active connections.")

            (some? connection-id-guess)
            (do
              (t/log! :debug ["Email seems to belong to the connection with the id" connection-id-guess])
              (if (true? (int/move-email-between-categories client connection-id-guess message-id old-category category context))
                (success-result :ok nil)
                (error-result nil "Moving email failed. Please check the logs.")))

            :else
            (let [results (for [conn connections
                                :let [id (get-in conn [:config :id])]]
                            (do (t/log! :debug ["Move message-id" message-id])
                                (int/move-email-between-categories client id message-id old-category category context)))]
              (if (some true? results)
                (success-result :ok nil)
                (error-result nil "Moving email failed. Please check the logs."))))

      (catch Exception e (t/log! :error e) (error-result e "Moving email failed. Please check the logs.")))))

(defn- move-message [move? folder connection-id email-message category {:keys [db client]}]
  (let [message-id (-> email-message :email :header :message-id)
        ;; The email stays in the folder it arrived in. Record that folder so a later recategorization
        ;; can find it even after the category's destination is changed.
        record-current-folder! (fn []
                                 (when (some? folder)
                                   (let [current-folder (int/current-folder-name client folder)]
                                     (when-not (str/blank? current-folder)
                                       (int/update-email-folder db message-id current-folder)))))]
    (if (and (true? move?) (some? category))
      (let [moved-to-folder (int/move-email-to-category client connection-id (:message email-message) folder category)]
        (if (string? moved-to-folder)
          (do (int/update-email-folder db message-id moved-to-folder)
              (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully moved to the corresponding folder"]))
          ;; The move did not complete (e.g. the copy+delete fallback failed). The email is still in
          ;; its source folder, so record that rather than leaving the location unknown.
          (do (t/log! :warn ["Email with subject:" (-> email-message :email :header :subject) "could not be moved; recording its current folder so it stays findable."])
              (record-current-folder!))))
      (do (t/log! :debug ["move option:" move? "category:" category "the email" (-> email-message :email :header :subject) "will not move moved"])
          (record-current-folder!)
          :na))))

(defn- incoming-email-workflow [email-message connection-id folder {:keys [move? assigned-category assigned-category-id]} {:keys [analyzer db] :as context}]
  (let [message-id (-> email-message :email :header :message-id)]
    (if (int/email-exists? db message-id)
      (t/log! :info ["Email" message-id "already in database — skipping re-categorization"])
      (if (some? assigned-category)
        (let [language-result (int/detect-language analyzer (:email email-message))
              enriched-email (core-email/construct-enriched-email (:email email-message) {:language (:code language-result) :language-confidence (:confidence language-result)} {:category assigned-category :category-id assigned-category-id :category-confidence 1})]
          (int/save-email db enriched-email)
          (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully saved to the database"])
          (move-message move? folder connection-id email-message assigned-category context))
        (let [enriched-email (int/enrich-email analyzer (:email email-message))
              category (:category (:metadata enriched-email))]
          (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was categorized as" category])
          (int/save-email db enriched-email)
          (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully saved to the database"])
          (move-message move? folder connection-id email-message category context))))))

(defn handle-incoming-imap-email
  "Handle incoming emails synchronously on a single thread. Returns a result."
  [parsed-email {:keys [connection-id origin-folder message] :as options} context]
  (try (let [process-result (incoming-email-workflow {:email parsed-email :message message} connection-id origin-folder options context)]
         (success-result :ok {:move process-result}))
       (catch Exception e (error-result e "Error encountered when processing incoming email"))))

(defn- process-nth-email-from-folder [client n folder-name folder options context messages-result]
  (try
    (let [email-message (int/nth-email-from-folder client n folder)]
      (incoming-email-workflow email-message (:connection-id messages-result) folder options context))
    (catch Exception e
      (t/log! :error ["Skipping email number" n "from folder" folder-name "because it could not be read or processed:" e])
      (error-result e "Error encountered when reading email from folder"))))

(defn read-emails-from-folder
  "Read all emails from a folder and process them. Returns the number of messages in the folder.
   Emails are read over a DEDICATED IMAP connection (separate from the IDLE monitor, so the two never
   contend for the same folder/connection) and processed on another thread."
  [connection-data folder-name options {:keys [client] :as context}]
  (let [bulk (int/open-folder-for-bulk-read client connection-data folder-name)
        folder (:folder bulk)
        message-count (:message-count bulk)]
    (if (> message-count 0)
      (do
        (t/log! :info ["There are" message-count "emails in" folder-name "The messages will get processed asynchronously"])
        ;; Use async/thread (a real thread), not async/go: each email does blocking JDBC and IMAP work,
        ;; and a long blocking loop inside a go block would tie up a shared core.async dispatch thread.
        (async/thread
          (try
            ;; Iterate sequence numbers high -> low. When move? is enabled a processed message is moved
            ;; out and expunged, which shifts the sequence numbers of all HIGHER messages down by one.
            ;; Going downward means the numbers we have not processed yet are never affected, so no
            ;; message is skipped or referenced after it moved. (Sequence numbers are 1-based.)
            (doseq [n (range message-count 0 -1)]
              (process-nth-email-from-folder client n folder-name folder options context bulk))
            (finally
              (int/close-folder-for-bulk-read client bulk)))))
      (do
        (int/close-folder-for-bulk-read client bulk)
        (t/log! :info ["There are no emails in the folder. Doing nothing."])))
    message-count))
