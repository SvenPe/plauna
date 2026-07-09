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

(defn- content->where
  "Build a where-clause matching e-mails whose body content contains search-text. A blank/nil
   search-text adds no filter. A non-correlated IN subquery (not a correlated EXISTS): the matching
   message-ids are resolved once instead of probing bodies for every header row — the count query
   re-runs the whole clause, so a correlated probe would run twice per page load."
  [search-text]
  (when-not (str/blank? search-text)
    [:in :headers.message-id
     {:select [:bodies.message-id] :from [:bodies] :where (like-contains :bodies.content search-text)}]))

(defn- contact-keys->where
  "Build a where-clause for e-mails with a participant of one of participant-types (e.g.
   [\"sender\" \":sender\"] — legacy rows may store the type with a leading colon, see the defensive
   strip in core.email/construct-participants). selection is {:include [...]} to match only those
   contact-keys, or {:exclude [...]} to match everything EXCEPT those contact-keys (the checklist UI
   submits whichever list is shorter — see emails.html's submitFilterForm — so a mailbox with
   hundreds of senders never has to send hundreds of query parameters just to exclude a few).
   Both empty adds no filter, same as every other filter field's 'blank means unfiltered' convention."
  [participant-types {:keys [include exclude]}]
  (cond
    (seq include)
    [:in :headers.message-id
     {:select [:communications.message-id]
      :from [:communications]
      :where [:and
              [:in :communications.type participant-types]
              [:in :communications.contact-key include]]}]

    (seq exclude)
    [:in :headers.message-id
     {:select [:communications.message-id]
      :from [:communications]
      :where [:and
              [:in :communications.type participant-types]
              [:not-in :communications.contact-key exclude]]}]

    :else nil))

(defn- sender-keys->where [selection] (contact-keys->where ["sender" ":sender"] selection))

(defn- recipient-keys->where [selection] (contact-keys->where ["receiver" ":receiver"] selection))

(defn- subject-values->where
  "selection is {:include [...]} to match only those subjects, or {:exclude [...]} to match every
   subject EXCEPT those (see contact-keys->where for why). Both empty adds no filter."
  [{:keys [include exclude]}]
  (cond
    (seq include) [:in :headers.subject include]
    (seq exclude) [:not-in :headers.subject exclude]
    :else nil))

(defn- date-string->epoch-seconds
  "Convert a 'YYYY-MM-DD' string to a UTC unix timestamp (seconds) at the start of that day,
   or the start of the following day when next-day? is true (used for an inclusive upper bound)."
  [date-str next-day?]
  (let [^java.time.LocalDate base (java.time.LocalDate/parse date-str)
        ^java.time.LocalDate day (if next-day? (.plusDays base 1) base)]
    (.toEpochSecond (.atStartOfDay day java.time.ZoneOffset/UTC))))

(defn- date->where
  "Build a where-clause filtering headers.date (stored as unix seconds) between the given dates.
   Either bound may be blank/nil. The upper bound is inclusive of the whole 'to' day. Uses the fully
   qualified column (rather than relying on database.clj's key-lookup postwalk, which only runs for
   the main e-mail-list query) so this fragment can also be reused directly in the checklist filters'
   scoped distinct-value queries — see other-filters-where."
  [date-from date-to]
  (let [from (when-not (str/blank? date-from) (date-string->epoch-seconds date-from false))
        to (when-not (str/blank? date-to) (date-string->epoch-seconds date-to true))]
    (cond
      (and from to) [:and [:>= :headers.date from] [:< :headers.date to]]
      from [:>= :headers.date from]
      to [:< :headers.date to]
      :else nil)))

(defn- combine-wheres
  "AND together the active where-clauses; nil (no filter at all) when none are active."
  [wheres]
  (let [active (remove nil? wheres)]
    (cond (empty? active) nil
          (= 1 (count active)) (first active)
          :else (into [:and] active))))

(def uncategorized-token
  "Sentinel value for the 'n/a' checkbox in the category filter, distinguishing 'no category was
   selected at all' (an absent/blank category-ids param) from 'the uncategorized bucket was selected'."
  "n-a")

(defn- category-tokens->numeric-ids
  "Parse every token except uncategorized-token as a category id; non-numeric tokens are dropped."
  [tokens]
  (keep (fn [token] (when (not= uncategorized-token token) (try (Integer/parseInt token) (catch NumberFormatException _ nil))))
        tokens))

(defn- category-ids->where
  "Build a where-clause for the category checklist. uncategorized-token stands for 'no category'
   (metadata.category IS NULL).
   - selection's :include matches only those categories.
   - selection's :exclude matches every category EXCEPT those (the checklist UI submits whichever
     list is shorter — see emails.html's submitFilterForm — so this only kicks in once more than
     half the checkboxes are checked, keeping the query string short either way).
   Both empty adds no filter, same as every other filter field's 'blank means unfiltered' convention
   (also how an Excel column filter behaves before you touch it: nothing unchecked yet)."
  [{:keys [include exclude]}]
  (let [include-uncategorized? (contains? (set include) uncategorized-token)
        include-ids (category-tokens->numeric-ids include)
        exclude-uncategorized? (contains? (set exclude) uncategorized-token)
        exclude-ids (category-tokens->numeric-ids exclude)]
    (cond
      (seq include)
      (cond
        (and include-uncategorized? (seq include-ids)) [:or [:in :metadata.category include-ids] [:= :metadata.category nil]]
        include-uncategorized? [:= :metadata.category nil]
        :else [:in :metadata.category include-ids])

      (seq exclude)
      (cond
        ;; Excluding "n/a" too: a real, non-excluded category is required.
        (and exclude-uncategorized? (seq exclude-ids)) [:and [:<> :metadata.category nil] [:not-in :metadata.category exclude-ids]]
        exclude-uncategorized? [:<> :metadata.category nil]
        ;; "n/a" is not excluded, so an uncategorized e-mail still passes alongside any non-excluded category.
        :else [:or [:not-in :metadata.category exclude-ids] [:= :metadata.category nil]])

      :else nil)))

(defn- success-result [result-type data] (conj {:result result-type} data))

(defn- error-result [exception alert-content] {:result :error :exception exception :message {:type :alert :content alert-content}})

(def default-category-color
  "Used for the 'n/a' pseudo-category and any category saved before color-coding existed."
  "#9ca3af")

(defn- valid-hex-color?
  "True for a '#' followed by exactly 6 hex digits, the format <input type=\"color\"> always submits.
   Rejects anything else so a hand-crafted request can't smuggle arbitrary text into the color
   attribute rendered on every category select."
  [color]
  (boolean (re-matches #"#[0-9a-fA-F]{6}" (or color ""))))

(defn- clean-color
  "A valid hex color, trimmed; nil for a blank or invalid one so it falls back to default-category-color."
  [color]
  (let [trimmed (when (string? color) (str/trim color))]
    (when (valid-hex-color? trimmed) trimmed)))

(defn categories
  "There is no entry for 'no entry' in the database. This function adds a 'n/a' entry to the actual list."
  [db] (conj (int/fetch-categories db) {:id nil :name "n/a" :color default-category-color}))

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

(defn- checklist-selection
  "Normalize a checklist filter's raw request parameters into {:include [...] :exclude [...]}.
   Exactly one side is ever populated by the UI (see emails.html's submitFilterForm), but both are
   defensively blank-filtered here."
  [parameters include-key exclude-key]
  {:include (remove str/blank? (get parameters include-key))
   :exclude (remove str/blank? (get parameters exclude-key))})

(defn- annotate-checked-by
  "Mark each item's :checked? state to match selection {:include [...] :exclude [...]}:
   - Both empty (the default, unfiltered state — nothing has been unchecked yet): every item checked.
   - :include non-empty: only items whose (key-fn item) appears in it are checked.
   - :exclude non-empty: every item is checked EXCEPT those whose (key-fn item) appears in it."
  [items {:keys [include exclude]} key-fn]
  (cond
    (seq include)
    (let [selected (set include)]
      (mapv (fn [item] (assoc item :checked? (contains? selected (str (key-fn item))))) items))

    (seq exclude)
    (let [excluded (set exclude)]
      (mapv (fn [item] (assoc item :checked? (not (contains? excluded (str (key-fn item)))))) items))

    :else
    (mapv (fn [item] (assoc item :checked? true)) items)))

(defn- other-filters-where
  "Combine every active filter's where-clause EXCEPT the one named by excluding (one of :category
   :subject :from :to). This scopes a column filter's checklist to values that are still reachable
   given every OTHER active filter — e.g. once a category is picked, the From checklist only offers
   senders who actually have mail in that category — the same way Excel's own AutoFilter narrows a
   column's dropdown as other filters are applied."
  [parameters excluding category-selection subject-selection from-selection to-selection]
  (combine-wheres
   [(filter->where (:filter parameters))
    (content->where (:search-text parameters))
    (when-not (= excluding :subject) (subject-values->where subject-selection))
    (when-not (= excluding :from) (sender-keys->where from-selection))
    (when-not (= excluding :to) (recipient-keys->where to-selection))
    (when-not (= excluding :category) (category-ids->where category-selection))
    (date->where (:date-from parameters) (:date-to parameters))]))

(defn- reachable-category-tokens
  "The set of category tokens (a numeric id, stringified, or uncategorized-token) that occur among
   headers matching other-filters-where."
  [db other-filters-where]
  (into #{} (map (fn [row] (str (or (:category row) uncategorized-token))))
        (int/fetch-header-categories db other-filters-where)))

(defn fetch-emails
  "Returns a list of emails. Customizable by parameters which can contain the following keys:
   :size, :page, :filter (all, enrieched-only, or without-category), :search-text (matches the
   e-mail body content), :date-from, :date-to, and for each Excel-style column filter (subject,
   from, to, category) an :xxx-values/:xxx-values-exclude (or :xxx-keys/:xxx-keys-exclude, or
   :category-ids/:category-ids-exclude) pair of collections — the checklist UI submits whichever
   one is shorter, so only one side is ever populated for a given filter.

   Each column filter's checklist of possible values is scoped to what's still reachable given every
   OTHER active filter (see other-filters-where), except the per-row category reassignment dropdown
   (:categories in the result), which always offers every category regardless of the list's filter."
  [context parameters]
  (let [db (:db context)
        category-selection (checklist-selection parameters :category-ids :category-ids-exclude)
        subject-selection (checklist-selection parameters :subject-values :subject-values-exclude)
        from-selection (checklist-selection parameters :from-keys :from-keys-exclude)
        to-selection (checklist-selection parameters :to-keys :to-keys-exclude)
        other-where (partial other-filters-where parameters)
        cat-list (annotate-checked-by (categories db) category-selection #(or (:id %) uncategorized-token))
        reachable-categories (reachable-category-tokens db (other-where :category category-selection subject-selection from-selection to-selection))
        category-filter-options (filterv #(contains? reachable-categories (str (or (:id %) uncategorized-token))) cat-list)
        subject-list (annotate-checked-by (int/fetch-distinct-subjects db (other-where :subject category-selection subject-selection from-selection to-selection)) subject-selection :subject)
        sender-list (annotate-checked-by (int/fetch-distinct-senders db (other-where :from category-selection subject-selection from-selection to-selection)) from-selection :contact_key)
        recipient-list (annotate-checked-by (int/fetch-distinct-recipients db (other-where :to category-selection subject-selection from-selection to-selection)) to-selection :contact_key)
        where (combine-wheres [(filter->where (:filter parameters))
                               (content->where (:search-text parameters))
                               (subject-values->where subject-selection)
                               (sender-keys->where from-selection)
                               (recipient-keys->where to-selection)
                               (category-ids->where category-selection)
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
                  :subject-values (:include subject-selection)
                  :subject-values-exclude (:exclude subject-selection)
                  :from-keys (:include from-selection)
                  :from-keys-exclude (:exclude from-selection)
                  :to-keys (:include to-selection)
                  :to-keys-exclude (:exclude to-selection)
                  :category-ids (:include category-selection)
                  :category-ids-exclude (:exclude category-selection)
                  :date-from (:date-from parameters)
                  :date-to (:date-to parameters)}
     :optional {:categories cat-list :category-filter-options category-filter-options
                :subjects subject-list :senders sender-list :recipients recipient-list}}))

(defn create-new-category! [context category destination-folder color]
  (let [db (:db context)
        client (:client context)
        cleaned (when-not (str/blank? destination-folder) (str/trim destination-folder))]
    (int/save-category db category cleaned (clean-color color))
    (doseq [connection-data (vals (int/connections client))]
      (int/create-category-directories! client connection-data [category]))))

(defn update-category!
  "Persist the destination folder and display color for a category, and make sure the folder exists on
   every active connection. A blank destination-folder restores the default 'Categories/<Name>'
   behaviour; an invalid or blank color falls back to default-category-color. Existing emails are not moved."
  [context id category-name destination-folder color]
  (let [db (:db context)
        client (:client context)
        cleaned (when-not (str/blank? destination-folder) (str/trim destination-folder))]
    (int/update-category db id cleaned (clean-color color))
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
  "Read emails from a folder and process them. Returns the number of messages in the folder.
   Emails are read over a DEDICATED IMAP connection (separate from the IDLE monitor, so the two never
   contend for the same folder/connection) and processed on another thread.

   options may include :limit N to process only the N most recent messages (the highest sequence
   numbers) instead of the whole folder. The reconnect back-fill uses this so a large mailbox isn't
   fully re-scanned every time; already-saved messages are skipped either way (see
   incoming-email-workflow), so a bounded scan still catches up every genuinely-missed message within
   the window."
  [connection-data folder-name options {:keys [client] :as context}]
  (let [bulk (int/open-folder-for-bulk-read client connection-data folder-name)
        folder (:folder bulk)
        message-count (:message-count bulk)
        limit (:limit options)
        ;; Lowest sequence number to process. With :limit, take only the top N (most recent) messages;
        ;; without it, everything down to 1. (Sequence numbers are 1-based; higher = more recent.)
        lowest (if limit (max 1 (- (inc message-count) limit)) 1)
        to-process (inc (- message-count lowest))]
    (if (> message-count 0)
      (do
        (t/log! :info ["There are" message-count "emails in" folder-name "-" to-process "will get processed asynchronously"])
        ;; Use async/thread (a real thread), not async/go: each email does blocking JDBC and IMAP work,
        ;; and a long blocking loop inside a go block would tie up a shared core.async dispatch thread.
        (async/thread
          (try
            ;; Iterate sequence numbers high -> low. When move? is enabled a processed message is moved
            ;; out and expunged, which shifts the sequence numbers of all HIGHER messages down by one.
            ;; Going downward means the numbers we have not processed yet are never affected, so no
            ;; message is skipped or referenced after it moved. (Sequence numbers are 1-based.)
            (doseq [n (range message-count (dec lowest) -1)]
              (process-nth-email-from-folder client n folder-name folder options context bulk))
            (finally
              (int/close-folder-for-bulk-read client bulk)))))
      (do
        (int/close-folder-for-bulk-read client bulk)
        (t/log! :info ["There are no emails in the folder. Doing nothing."])))
    message-count))
