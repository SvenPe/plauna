(ns plauna.interfaces)

(defprotocol DB
  "Database protocol"
  (fetch-connection [this id] "Get connection for id.")
  (fetch-oauth-token-data [this id] "Get oauth token data for a connection")
  (fetch-auth-provider [this id])
  (fetch-categories [this] "Get a list of all categories")
  (fetch-emails [this entity customization] "Get a list of emails")
  (save-category [this category-name destination-folder])
  (update-category-destination-folder [this id destination-folder] "Set the IMAP folder that emails of this category should be moved to. A blank value restores the default 'Categories/<Name>' behaviour.")
  (update-email-folder [this message-id folder] "Record the IMAP folder a message currently lives in.")
  (email-exists? [this message-id] "Return true if a header with this message-id is already in the database.")
  (save-email [this email]))

(defprotocol EmailClient
  "Email client"
  (start-monitor [this config context] "Connect to the client")
  (connections [this] "Get a list of connections")
  (create-category-directories! [this connection-data category-names])
  (connection-id-for-email [this connections email])
  (move-email-between-categories [this connection-id message-id old-category new-category context])
  (move-email-to-category [this connection-id original-message original-folder category])
  (nth-email-from-folder [this n folder])
  (current-folder-name [this folder] "Return the full IMAP name of a folder object.")
  (open-folder-for-bulk-read [this connection-data folder-name] "Open folder-name on a dedicated Store connection, isolated from the IDLE monitor. Returns a handle {:folder :message-count :connection-id ...}.")
  (close-folder-for-bulk-read [this bulk-handle] "Close the dedicated connection opened by open-folder-for-bulk-read."))

(defprotocol Analyzer
  "Language detection and categorization"
  (enrich-email [this email])
  (detect-language [this email]))
