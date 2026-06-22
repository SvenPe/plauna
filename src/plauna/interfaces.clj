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
  (save-email [this email]))

(defprotocol EmailClient
  "Email client"
  (start-monitor [this config context] "Connect to the client")
  (connections [this] "Get a list of connections")
  (create-category-directories! [this connection-data category-names])
  (connection-id-for-email [this connections email])
  (move-email-between-categories [this connection-id message-id old-category new-category context])
  (move-email-to-category [this connection-id original-message original-folder category])
  (number-of-messages-in-folder [this connection-data folder-name])
  (nth-email-from-folder [this n folder])
  (current-folder-name [this folder] "Return the full IMAP name of a folder object.")
  (pause-monitoring-for-folder [this connection-data folder-name] "If folder-name is the connection's monitored folder, stop IDLE monitoring (and its health-check re-arming) and return true; otherwise return false.")
  (resume-monitoring [this connection-data context] "Restart IDLE monitoring and health checks for the connection."))

(defprotocol Analyzer
  "Language detection and categorization"
  (enrich-email [this email])
  (detect-language [this email]))
