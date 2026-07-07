(ns plauna.client
  (:require
   [plauna.preferences :as p]
   [plauna.database :as db]
   [plauna.client.oauth :as oauth]
   [clojure.string :as s]
   [taoensso.telemere :as t]
   [plauna.interfaces :as int]
   [plauna.application :as app])
  (:import
   (plauna.core.email Header Body-Part Participant Email)
   (clojure.lang PersistentVector)
   (jakarta.mail Store Session Folder BodyPart Multipart Message Message$RecipientType Flags$Flag AuthenticationFailedException)
   (jakarta.mail.internet InternetAddress)
   (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
   (jakarta.mail.event MessageCountAdapter MessageCountEvent MessageCountListener)
   (jakarta.mail.search MessageIDTerm)
   (java.lang AutoCloseable)
   (java.util Properties UUID)
   (java.util.concurrent Executors)
   (org.eclipse.angus.mail.imap IdleManager IMAPStore)
   (java.util.concurrent Executors TimeUnit ScheduledExecutorService ScheduledFuture)))

(set! *warn-on-reflection* true)

;; A small pool rather than a single thread: each connection's health check does blocking IMAP/HTTP
;; work, so one slow or hung connection on a single-thread scheduler would stall every other
;; connection's health check.
(defonce executor-service (Executors/newScheduledThreadPool 4))

(defonce parent-folder-name "Categories")

(defonce connections (atom {}))

(defonce health-checks (atom {}))

(declare connect)

(declare reconnect)

(declare start-monitoring)

(declare stop-monitoring)

(declare schedule-health-checks)

(defn default-port-for-security [security]
  (if (= security "ssl") 993 143))

(defn oauth2? [connection-config] (= "oauth2" (:auth-type connection-config)))

(defn security [connection-config]
  (let [security (get connection-config :security "ssl")]
    (if (some #(= security %) ["ssl" "starttls" "plain"])
      security
      "ssl")))

(defn port [connection-config]
  (str (get connection-config :port (default-port-for-security (security connection-config)))))

(defn check-ssl-certs? [connection-config] (get connection-config :check-ssl-certs true))

(defn default-imap-properties ^Properties [connection-config]
  (doto (new Properties)
    (.setProperty "mail.imap.port" (port connection-config))
    (.setProperty "mail.imap.usesocketchannels" "true")
    (.setProperty "mail.imap.timeout" "5000")
    (.setProperty "mail.imap.partialfetch" "false")
    (.setProperty "mail.imap.fetchsize" "1048576")))

(defn oauth-properties [connection-config]
  (fn [^Properties properties]
    (if (oauth2? connection-config)
      (doto properties (.setProperty "mail.imap.auth.mechanisms" "XOAUTH2"))
      properties)))

(defn security-properties [connection-config]
  (let [security-key (security connection-config)]
    (fn [^Properties properties]
      (cond (= security-key "ssl") (doto properties (.setProperty "mail.imap.ssl.enable" "true"))
            (= security-key "starttls") (doto properties (.setProperty "mail.imap.starttls.enable" "true"))
            (= security-key "plain") properties
            :else (doto properties (.setProperty "mail.imap.ssl.enable" "true"))))))

(defn certification-check-properties [connection-config]
  (if (not (check-ssl-certs? connection-config))
    (fn [^Properties properties] (doto properties (.setProperty "mail.imap.ssl.trust" "*")))
    (fn [^Properties properties] properties)))

(defn set-debug-mode [connection-config]
  (let [debug? (get connection-config :debug false)]
    (fn [^Session session]
      (if debug? (doto session (.setDebug true)) session))))

(defn config->session [connection-config]
  (-> (default-imap-properties connection-config)
      ((security-properties connection-config))
      ((oauth-properties connection-config))
      ((certification-check-properties connection-config))
      Session/getInstance
      ((set-debug-mode connection-config))))

(defn connection-config->store [connection-config]
  ;; Always use the "imap" store. SSL is enabled via mail.imap.ssl.enable (see security-properties), not
  ;; by switching to the "imaps" store. This matters because IdleManager requires the folder to use
  ;; socket channels (mail.imap.usesocketchannels=true), and that — along with every other property here —
  ;; lives under the mail.imap.* prefix. The "imaps" store reads mail.imaps.* instead, so usesocketchannels
  ;; would be ignored and IdleManager.watch would fail with "Folder is not using SocketChannels".
  (.getStore ^Session (config->session connection-config) "imap"))

(defn login
  ([connection-config ^Store store]
   (if (oauth2? connection-config)
     (let [tokens (db/get-oauth-tokens (:id connection-config))]
       (.connect store (:host connection-config) (:user connection-config) (:access-token tokens)))
     (.connect store (:host connection-config) (:user connection-config) (:secret connection-config)))
   store)
  ([connection-config]
   (login connection-config (connection-config->store connection-config))))

(defn folder-separator [^Store store] (.getSeparator (.getDefaultFolder store)))

(defn create-folder [^Store store ^String folder-name result-map]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (if (not (.exists folder))
      (do (.create folder Folder/HOLDS_MESSAGES)
          (conj result-map {folder-name :created}))
      (conj result-map {folder-name :already-exists}))))

(defn default-category-folder-name
  "The default folder for a category, ignoring any custom destination: 'Categories/<Name>'."
  [store lower-case-folder-name]
  (str parent-folder-name (folder-separator store) (s/capitalize lower-case-folder-name)))

(defn structured-folder-name
  "The folder a category's mail should be moved to: the custom destination if configured, otherwise the default 'Categories/<Name>'."
  [store lower-case-folder-name]
  (let [destination-folder (:destination_folder (db/category-by-name lower-case-folder-name))]
    (if (s/blank? destination-folder)
      (default-category-folder-name store lower-case-folder-name)
      destination-folder)))

(defn create-folders
  ([store folder-names]
   (create-folders store folder-names {}))
  ([store folder-names result-map]
   (if (empty? folder-names)
     result-map
     (let [result (create-folder store (structured-folder-name store (first folder-names)) result-map)]
       (recur store (rest folder-names) result)))))

(defn swap-new-period-check [identifier future]
  ;; Cancel any health check already scheduled for this connection before replacing it; otherwise a
  ;; reconnect orphans the previous ScheduledFuture, which keeps running forever on the shared executor.
  (when-let [^ScheduledFuture existing (get @health-checks identifier)]
    (.cancel existing true))
  (swap! health-checks assoc identifier future))

;; Primitives

(defn clean-config [config]
  (-> (dissoc config :secret)
      (dissoc :debug)))

(defn id-from-config [config]
  (str (UUID/nameUUIDFromBytes (.getBytes ^String (str (hash (clean-config config)))))))

(defrecord ConnectionData [config ^Store store ^Folder folder ^IdleManager idle-manager capabilities ^MessageCountListener message-count-listener]
  AutoCloseable
  (close [this]
    (t/log! :info "Closing the idle manager, removing from health checks, closing the folder and the store.")
    (.stop idle-manager)
    (stop-monitoring this)
    (swap! health-checks dissoc (:id config))
    (when (.isOpen folder)
      (.close folder))
    (.close store)))

(defn get-connections [] (vals @connections))

(defn connection-data-from-id ^ConnectionData [id]
  (get @connections id))

(defn add-to-connections [^ConnectionData connection-data]
  (swap! connections conj {(:id (.config connection-data)) connection-data}))

;; Construct email from message

(defn text? [content-type] (s/starts-with? (s/lower-case content-type) "text"))

(defn mime-type [content-type] (s/lower-case (first (s/split content-type #";"))))

(defonce fallback-charset "us-ascii")

(defn charset [content-type]
  ;; Extract the charset parameter wherever it appears (handling optional quotes). A text/* part is not
  ;; required to declare a charset (e.g. "Content-Type: text/plain"); fall back rather than NPE when the
  ;; charset parameter is absent or malformed.
  (or (when (text? content-type)
        (some-> (re-find #"(?i)charset\s*=\s*\"?([^\";\s]+)" content-type)
                second
                s/lower-case))
      fallback-charset))

(defn disposition [disposition] (when (some? disposition) (s/lower-case disposition)))

(defn create-header [^IMAPMessage message]
  (let [sent (.getSentDate message)]
    (new Header (.getMessageID message) (.getInReplyTo message) (.getSubject message)
                (mime-type (.getContentType message))
                (when sent (quot (.getTime sent) 1000)))))

(defmulti create-body-part (fn [body-part _] (type body-part)))

(defmethod create-body-part String [content ^IMAPMessage message]
  (new Body-Part (.getMessageID message) (charset (.getContentType message)) (mime-type (.getContentType message)) (first (.getHeader message "Content-transfer-encoding")) content (.getFileName message) (.getDisposition message)))

(defmethod create-body-part BodyPart [^BodyPart bodypart ^IMAPMessage message]
  (let [content-type (.getContentType bodypart)
        content (.getContent bodypart)]
    (if (instance? Multipart content)
      (create-body-part content message)
      (new Body-Part (.getMessageID message) (charset content-type) (mime-type content-type) (first (.getHeader bodypart "Content-transfer-encoding"))
           ;; Only persist textual content (as a String). For attachments (PDFs, images, ...) JavaMail
           ;; returns the content as an InputStream; storing that bloats the DB and, on MariaDB, fails the
           ;; insert outright (leaving a header with no body parts). Attachments are intentionally not
           ;; stored, mirroring the mbox parser.
           (when (and (text? content-type) (string? content)) content)
           (.getFileName bodypart) (disposition (.getDisposition bodypart))))))

(defmethod create-body-part :default [_ ^IMAPMessage message]
  ;; A non-multipart message whose body is neither a String nor a recognised part (e.g. a bare
  ;; attachment): keep its metadata but do not store the (binary) content.
  (new Body-Part (.getMessageID message) (charset (.getContentType message)) (mime-type (.getContentType message)) (first (.getHeader message "Content-transfer-encoding")) nil (.getFileName message) (.getDisposition message)))

(defmethod create-body-part Multipart [^Multipart multipart ^IMAPMessage message]
  (mapv (fn [i] (create-body-part (.getBodyPart multipart i) message))
        (range 0 (.getCount multipart))))

(defn- realize-body-parts [body-parts]
  (vec (flatten [body-parts])))

;; TODO remove duplication with parser.clj
(defn uuid [^String name] (str (java.util.UUID/nameUUIDFromBytes (.getBytes name))))

(defmulti create-participant (fn [address _ _] (type address)))

(defmethod create-participant InternetAddress [^InternetAddress address contact-type message-id]
  (let [name (.getPersonal address)
        address (.getAddress address)
        contact-key (uuid (str name address))]
    (new Participant address name contact-key contact-type message-id)))

(defn create-participants [^IMAPMessage message]
  (let [sender (.getSender message)
        message-id (.getMessageID message)
        sender-participant (when sender (create-participant sender :sender message-id))
        recipient-participants (mapv (fn [address] (create-participant address :receiver message-id)) (.getRecipients message Message$RecipientType/TO))
        cc-participants (mapv (fn [address] (create-participant address :cc message-id)) (.getRecipients message Message$RecipientType/CC))
        bcc-participants (mapv (fn [address] (create-participant address :bcc message-id)) (.getRecipients message Message$RecipientType/BCC))]
    (filterv some? (flatten [sender-participant recipient-participants cc-participants bcc-participants]))))

(defn message->email [^IMAPMessage message]
  (new Email
       (create-header message)
       (realize-body-parts (create-body-part (.getContent message) message))
       (create-participants message)))

;; Calls

(defn capability-name [^IMAPStore store ^String cap-name]
  (when (.hasCapability store cap-name)
    (keyword (clojure.string/lower-case cap-name))))

(defn capabilities [^Store store]
  (filterv some? (mapv #(capability-name store %) ["MOVE"])))

(defn start-idling-for-id [id]
  (let [^ConnectionData connection-data (connection-data-from-id id)]
    (t/log! :debug ["Starting to idle for id:" id "using connection-data" connection-data])
    (.watch ^IdleManager (.idle-manager connection-data) (.folder connection-data))))

(defn message-count-listener [connection-id folder folder-name context]
  (proxy [MessageCountAdapter] []
    (messagesAdded [^MessageCountEvent event]
      (t/log! :debug "Received new message event.")
      (doseq [message ^IMAPMessage (.getMessages event)]
        (t/log! :debug ["Processing message:" message])
        (.setPeek ^IMAPMessage message true)
        (let [parsed-email (message->email message)
              process (app/handle-incoming-imap-email parsed-email
                                                      {:connection-id connection-id :origin-folder folder :message message :move? true}
                                                      context)]
          (if (= :error (:result process))
            (t/log! :error ["An error occured while handling incoming message" (:exception process)])
            (t/log! :info ["The email with subject" (-> parsed-email :header :subject) "was handled successfully"])))
        (let [conn-data ^ConnectionData (connection-data-from-id connection-id)]
          (t/log! :debug ["Idling on the folder" folder-name "while waiting for new messages."])
          (.watch ^IdleManager (.idle-manager conn-data) (.folder conn-data)))))))

(defn open-folder-in-store [^Store store ^String folder-name]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (when (not (.isOpen folder))
      (.open folder Folder/READ_WRITE))
    folder))

(defn copy-message
  "Copy a message to the target folder and delete it from the source. Returns true on confirmed success, false if any step failed."
  [^Message message ^Folder source-folder ^Folder target-folder]
  (try
    (.setPeek ^IMAPMessage message true)
    (.copyMessages source-folder (into-array Message [message]) target-folder)
    (t/log! :debug ["Copied" message])
    (.setFlag message Flags$Flag/DELETED true)
    (t/log! :debug ["Set DELETED flag for" message])
    ;; Expunge ONLY the message we copied. The no-arg expunge would remove every DELETED-flagged
    ;; message in the folder, including unrelated ones a concurrent operation may have flagged.
    (.expunge ^IMAPFolder source-folder (into-array Message [message]))
    (t/log! :debug ["Expunged source folder"])
    true
    (catch Exception e (t/log! {:level :error :error e} ["There was an error copying and deleting the message" message])
           false)))

(defn inbox-or-category-folder-name [^Store store ^String folder-name default]
  (let [real-default (if (s/blank? default) "INBOX" default)]
    (if (nil? folder-name) real-default (structured-folder-name store folder-name))))

(defn inbox-or-default-category-folder-name
  "Like inbox-or-category-folder-name but resolves a category to its DEFAULT folder, ignoring any custom destination.
   Used to locate emails that were filed before per-email folder tracking existed: they always live under the default scheme."
  [^Store store ^String folder-name default]
  (let [real-default (if (s/blank? default) "INBOX" default)]
    (if (nil? folder-name) real-default (default-category-folder-name store folder-name))))

(defn move-message
  "Find the proper location for the email and move it there. Returns the name of the folder to which the email was moved."
  [connection-id ^Message message ^Folder source-folder ^String target-name]
  (let [connection-data (connection-data-from-id connection-id)
        ;; Resolve and open the target folder from the SAME Store as the source folder. moveMessages and
        ;; the copy fallback cannot operate across two different Stores, and during a bulk parse the source
        ;; lives in a dedicated bulk-read Store rather than the monitor's Store.
        store ^Store (.getStore source-folder)
        capabilities ^PersistentVector (:capabilities connection-data)
        structured-folder (inbox-or-category-folder-name store target-name (-> connection-data :config :folder))
        target-folder ^IMAPFolder (.getFolder ^Store store ^String structured-folder)]
    (cond
      (= (.getFullName source-folder) structured-folder)
      (do (t/log! :debug ["Target folder" structured-folder "is the same as the source folder. Leaving the message in place."])
          structured-folder)

      (.contains capabilities :move)
      (do (t/log! :debug ["Moving message from" source-folder "to" target-folder])
          (.setPeek ^IMAPMessage message true)
          (.moveMessages ^IMAPFolder source-folder (into-array Message [message]) target-folder)
          structured-folder)

      :else
      (do (t/log! :debug "Server does not support the IMAP MOVE command. Using copy and delete as fallback.")
          ;; Only report the new folder if the copy+delete actually succeeded, so a failed
          ;; fallback is never recorded as a completed move.
          (when (copy-message message source-folder target-folder)
            structured-folder)))))

(defn monitor->map [monitor]
  (if (nil? monitor)
    {:connected false :folder-open false}
    (let [store ^Store (-> monitor :store)
          folder ^IMAPFolder (-> monitor :folder)]
      {:connected (.isConnected ^Store store)
       :folder-open    (.isOpen ^IMAPFolder folder)})))

(defn folders-in-store [^Store store]
  (.list (.getDefaultFolder store) "*"))

(defn connected? [^ConnectionData connection-data] (.isConnected ^Store (:store connection-data)))

(defn disconnected-connections
  "Returns configured connections (from DB) that are not currently connected.
   A connection is disconnected when it has no active ConnectionData or its store reports false."
  []
  (let [active @connections]
    (filterv (fn [conn]
               (let [cd (get active (:id conn))]
                 (or (nil? cd) (not (connected? cd)))))
             (db/get-connections))))

(defn- set-message-as-peek [^IMAPMessage message] (.setPeek message true))

(defn- set-messages-as-peek [messages] (doseq [message messages] (set-message-as-peek message)))

(defn move-messages-by-id-between-category-folders
  "Return true if the message could be moved. False if not."
  [^String id message-id ^String source-name ^String target-name context]
  (let [^ConnectionData connection-data (connection-data-from-id id)]
    (if (connected? connection-data)
      (let [^Store store (:store connection-data)
            ^String source-folder-name (let [recorded-folder (db/email-folder message-id)]
                                         (if (s/blank? recorded-folder)
                                           ;; No recorded folder: the email predates folder tracking, so it lives under
                                           ;; the DEFAULT category folder, never a (newer, possibly-changed) custom destination.
                                           (inbox-or-default-category-folder-name store source-name (-> connection-data :config :folder))
                                           recorded-folder))
            ^String target-folder-name (inbox-or-category-folder-name store target-name (-> connection-data :config :folder))]
        (if (= source-folder-name target-folder-name)
          (do (t/log! :info ["Source and target folder are both" target-folder-name "- leaving the message in place."])
              ;; Even though nothing moves, record the resolved folder so a previously-unrecorded
              ;; (legacy) email gets a concrete location and stays findable for future moves.
              (db/update-email-folder message-id target-folder-name)
              true)
          (with-open [^IMAPFolder target-folder (open-folder-in-store store target-folder-name)
                    ^IMAPFolder source-folder (open-folder-in-store store source-folder-name)]
          (let [found-messages (.search source-folder (MessageIDTerm. message-id))]
            (t/log! :debug ["Found" (count found-messages) "messages when searched for the message-id:" message-id])
            (if (some? (seq found-messages))
              (if (= target-folder-name (:folder (:config connection-data)))
                (do
                  (stop-monitoring connection-data)
                  ;; The move happens on the monitored folder, so monitoring is paused first. Use
                  ;; try/finally so a failure mid-move can never leave monitoring permanently off.
                  ;; stop-monitoring also cancels the periodic health check, so it must be
                  ;; rescheduled here — otherwise the connection silently loses auto-reconnect.
                  (try
                    (set-messages-as-peek found-messages)
                    (t/log! :debug ["Moving e-mail from" source-folder-name "to" target-folder-name])
                    (.moveMessages source-folder (into-array Message found-messages) target-folder)
                    (db/update-email-folder message-id target-folder-name)
                    true
                    (finally
                      (schedule-health-checks (start-monitoring connection-data context)))))
                (do
                  (set-messages-as-peek found-messages)
                  (t/log! :debug ["Moving e-mail from" source-folder-name "to" target-folder-name])
                  (.moveMessages source-folder (into-array Message found-messages) target-folder)
                  (db/update-email-folder message-id target-folder-name)
                  true))
              (do (t/log! :info ["No messages found in" source-folder-name "in store" (.getURLName store)])
                  false))))))
      (do
        (t/log! :info ["IMAP store in connection" (:id (:config connection-data)) "is not connected. Cancelling the move attempt."])
        false))))

(defn- invalid-grant-error?
  "True only when the provider explicitly rejected the refresh token (HTTP 4xx with an
   invalid_grant error), as opposed to a transient network/5xx error or a timeout."
  [e]
  (let [{:keys [status body]} (ex-data e)]
    (boolean (and status (<= 400 status 499)
                  (re-find #"invalid_grant" (str body))))))

(defn refresh-access-token [connection-config]
  (let [provider (db/get-auth-provider (:auth-provider connection-config))
        token-data (db/get-oauth-tokens (:id connection-config))
        result (try {:token (oauth/exchange-refresh-token-for-access-token provider (:refresh-token token-data))}
                    (catch Exception e {:error e}))]
    (cond
      (some? (:token result))
      (db/update-access-token (:id connection-config) (:token result))

      (invalid-grant-error? (:error result))
      (do (t/log! :info ["Refresh token was rejected by the provider (invalid_grant). Deleting the stored token; the user must log in manually again."])
          (db/delete-access-token (:id connection-config)))

      :else
      ;; Transient failure (network, 5xx, timeout, or empty response): keep the refresh token and retry on the next cycle.
      (t/log! {:level :error :error (:error result)}
              ["Could not refresh the access token due to a transient error. Keeping the stored refresh token to retry later."]))))

(defn monitor-folder-name [folder-name]
  (if (or (nil? folder-name) (s/blank? folder-name)) "INBOX" folder-name))

;; Public Interface

(defn construct-connection-data [connection-config context]
  (let [idle-manager (IdleManager. (config->session connection-config) (Executors/newCachedThreadPool))
        store (login connection-config)
        id (:id connection-config)
        folder-name-to-monitor (monitor-folder-name (:folder connection-config))
        folder (open-folder-in-store store folder-name-to-monitor)
        listener (message-count-listener id folder folder-name-to-monitor context)
        connection-data (->ConnectionData connection-config store folder idle-manager (capabilities store) listener)]
    (add-to-connections connection-data)
    connection-data))

(defn disconnect [^AutoCloseable connection-data] (.close connection-data))

(defn disconnect-all [] (doseq [connection (vals @connections)] (disconnect connection)))

(defn reconnect [^ConnectionData connection-data]
  (try
    (t/log! :info ["Trying to reconnect to" (-> connection-data .config :host) "as" (-> connection-data .config :user)])
    (if (connected? connection-data)
      (do
        (t/log! :info ["IMAP store is already connected for" (-> connection-data .config :user) "- leaving it open."])
        connection-data)
      (do
        (when (oauth2? (.config connection-data)) (refresh-access-token (.config connection-data)))
        (login (.config connection-data) (.store connection-data))))
    (catch AuthenticationFailedException e (t/log! :error e))))

(defn start-monitoring [connection-data _context]
  (try
    ;; Attach the listener stored in the ConnectionData record — the same instance stop-monitoring
    ;; removes. Attaching a freshly created listener here would make the removal in stop-monitoring a
    ;; no-op, so every stop/start cycle (e.g. a category move on the monitored folder) would stack one
    ;; more listener and each new email would get processed once per stacked listener.
    (.addMessageCountListener ^IMAPFolder (:folder connection-data) ^MessageCountListener (:message-count-listener connection-data))
    (t/log! :info ["Started monitoring for" (:folder (:config connection-data)) "in" (.getURLName ^Store (:store connection-data))])
    (.watch ^IdleManager (:idle-manager connection-data) ^Folder (:folder connection-data))
    (catch Exception e
      (t/log! {:level :error :error e} (.getMessage e))))
  connection-data)

(defn stop-monitoring [connection-data]
  (t/log! :info ["Removing message count listener from folder" (-> connection-data :config :folder)])
  (let [connection-id (:id (:config connection-data))
        sf ^ScheduledFuture (get @health-checks connection-id)]
    (when (some? sf) (.cancel sf true)))
  (.removeMessageCountListener ^IMAPFolder (:folder connection-data) (:message-count-listener connection-data))
  connection-data)

(defn create-category-folders!
  "Creates folders for the selected categories. Checks if the connection is still intact. Does nothing, if the connection is not intact."
  [connection-data categories]
  (if (connected? connection-data)
    (do (t/log! :info ["Creating directories from category names" categories])
        (let [result (create-folders (:store connection-data) categories)]
          (t/log! {:level :info
                   :data  {:result result}}
                  "Created the directories.")))
    (t/log! :info "Could not create directories on the IMAP server: The store is not connected."))
  connection-data)

(defn schedule-health-checks [^ConnectionData connection-data]
  (let [^Store store (:store connection-data)
        ^Folder folder (:folder connection-data)
        config (:config connection-data)
        scheduled-future (.scheduleAtFixedRate ^ScheduledExecutorService executor-service
                                               #(do
                                                  (try
                                                    (t/log! :debug ["Checking if the connection for" (:user config) "is open"])
                                                    (if (.isConnected store)
                                                      (t/log! :debug "Store is still connected.")
                                                      (do
                                                        (t/log! :warn "Connection lost. Reconnecting to email server...")
                                                        (reconnect connection-data)))
                                                    (t/log! :debug ["Checking if the folder " (:folder config) "is open"])
                                                    (if (.isOpen folder)
                                                      (t/log! :debug "Folder is still open.")
                                                      (do (t/log! :info "Folder is closed. Opening it again.")
                                                          (.open folder Folder/READ_WRITE)))
                                                    (t/log! :debug "Idling and waiting for messages after a health check.")
                                                    (start-idling-for-id (:id config))
                                                    (catch Exception e
                                                      (t/log! {:level :error :error e} "There was an error during health check. The connection is probably in a broken state."))))
                                               120 (p/client-health-check-interval) TimeUnit/SECONDS)]
    (swap-new-period-check (:id config) scheduled-future)
    connection-data))

(defn folder-from-connection [connection-data folder-name]
  (let [^Store store (:store connection-data)]
    (open-folder-in-store store folder-name)))

(defn id-from-connection [connection-data] (get-in connection-data [:config :id]))

(defn open-folder-for-bulk-read
  "Open folder-name on a DEDICATED Store connection, separate from the monitoring connection.
   Bulk reading and the IDLE monitor therefore never share a connection or folder object, which
   avoids deadlocking the monitored folder's IDLE. Returns a handle to close with close-folder-for-bulk-read."
  [connection-data folder-name]
  (let [connection-config (:config connection-data)]
    (when (oauth2? connection-config) (refresh-access-token connection-config))
    (let [^Store store (login connection-config)]
      (try
        (let [^IMAPFolder folder (open-folder-in-store store folder-name)]
          {:store store
           :folder folder
           :message-count (.getMessageCount folder)
           :connection-id (id-from-connection connection-data)})
        (catch Exception e
          ;; Opening/counting failed - don't leak the just-opened Store.
          (try (.close store) (catch Exception _ nil))
          (throw e))))))

(defn close-folder-for-bulk-read [bulk-handle]
  (let [^Folder folder (:folder bulk-handle)
        ^Store store (:store bulk-handle)]
    (when (some? folder)
      (try (when (.isOpen folder) (.close folder false))
           (catch Exception e (t/log! {:level :error :error e} "Error closing bulk-read folder"))))
    (when (some? store)
      (try (.close store)
           (catch Exception e (t/log! {:level :error :error e} "Error closing bulk-read store"))))))

(defn refetch-message-by-id
  "Re-read a single message from the IMAP server by its Message-ID and return a freshly parsed Email,
   or nil if it cannot be found on any connected account. Searches the email's recorded folder first,
   then each account's monitored folder and INBOX. Uses a dedicated bulk-read connection so the IDLE
   monitor is left undisturbed."
  [message-id]
  (let [recorded (db/email-folder message-id)
        candidate-folders (fn [^ConnectionData cd]
                            (distinct (remove s/blank? [recorded (-> cd :config :folder) "INBOX"])))]
    (some (fn [^ConnectionData cd]
            (when (connected? cd)
              (some (fn [folder-name]
                      (let [bulk (try (open-folder-for-bulk-read cd folder-name)
                                      (catch Exception e
                                        (t/log! {:level :warn :error e} ["Could not open folder" folder-name "while re-fetching" message-id])
                                        nil))]
                        (when bulk
                          (try
                            (when-let [^IMAPMessage msg (first (.search ^IMAPFolder (:folder bulk) (MessageIDTerm. message-id)))]
                              (set-message-as-peek msg)
                              (message->email msg))
                            (finally (close-folder-for-bulk-read bulk))))))
                    (candidate-folders cd))))
          (vals @connections))))

(defmulti connect (fn [config _] (:auth-type config)))

(defmethod connect "oauth2" [connection-config context]
  (refresh-access-token connection-config)
  (try (-> (construct-connection-data connection-config context)
           (start-monitoring context)
           schedule-health-checks)
       (catch AuthenticationFailedException e (t/log! :error e))))

(defmethod connect :default [connection-config context]
  (-> (construct-connection-data connection-config context)
      (start-monitoring context)
      schedule-health-checks))

(defn connection-id-for-email
  "Tries to find out the id of the connection the email belongs to. Returns nil if no active connection is found."
  [connection-data-vec email]
  (loop  [connections connection-data-vec
          result nil]
    (if (or (some? result) (nil? (seq connections)))
      result
      (let [^ConnectionData connection (first connections)
            recipients (filterv #(= :receiver (:type %)) (:participants email))
            connection-user (get-in connection [:config :user])
            match (get (filterv (fn [sender] (= (:address sender) connection-user)) recipients) 0)]
        (if (some? match) (recur (rest connections) (get-in connection [:config :id])) (recur (rest connections) nil))))))

(defrecord ImapClient []
  int/EmailClient
  (start-monitor [_ config context] (connect config context))
  (connections [_] @connections)
  (create-category-directories! [_ connection-data category-names] (create-category-folders! connection-data category-names))
  (connection-id-for-email [_ connections email] (connection-id-for-email connections email))
  (move-email-between-categories [_ connection-id message-id old-category new-category context] (move-messages-by-id-between-category-folders connection-id message-id old-category new-category context))
  (move-email-to-category [_ connection-id message original-folder category] (move-message connection-id message original-folder category))
  (current-folder-name [_ folder] (.getFullName ^Folder folder))
  (open-folder-for-bulk-read [_ connection-data folder-name] (open-folder-for-bulk-read connection-data folder-name))
  (close-folder-for-bulk-read [_ bulk-handle] (close-folder-for-bulk-read bulk-handle))
  (nth-email-from-folder [_ n folder]
    (let [message (.getMessage ^IMAPFolder folder n)]
      (set-message-as-peek message)
      (t/log! :debug ["Reading message number" n "from" (.getName ^IMAPFolder folder)])
      {:email (message->email message)
       :message message})))
