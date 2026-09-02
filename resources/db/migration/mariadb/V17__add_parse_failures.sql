-- Messages that could not be read or processed during a folder parse or the reconnect back-fill.
-- They were never saved, so without this list there is no trace of them apart from the log.
-- The IMAP UID identifies a message within its folder even after other messages were moved away.
CREATE TABLE IF NOT EXISTS parse_failures (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    connection_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    folder TEXT NOT NULL,
    uid BIGINT,
    message_number INT,
    message_id TEXT,
    subject TEXT,
    error TEXT,
    attempts INT NOT NULL DEFAULT 1,
    first_seen BIGINT,
    last_seen BIGINT
);

CREATE INDEX IF NOT EXISTS idx_parse_failures_connection_uid ON parse_failures(connection_id, uid);
