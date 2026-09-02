-- Messages that could not be read or processed during a folder parse or the reconnect back-fill.
-- They were never saved, so without this list there is no trace of them apart from the log.
-- The IMAP UID identifies a message within its folder even after other messages were moved away.
CREATE TABLE parse_failures(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       connection_id TEXT NOT NULL,
       folder TEXT NOT NULL,
       uid INTEGER,
       message_number INTEGER,
       message_id TEXT,
       subject TEXT,
       error TEXT,
       attempts INTEGER NOT NULL DEFAULT 1,
       first_seen INTEGER,
       last_seen INTEGER);

CREATE INDEX IF NOT EXISTS idx_parse_failures_connection_folder ON parse_failures(connection_id, folder, uid);
