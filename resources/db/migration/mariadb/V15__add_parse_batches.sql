-- One row per manual "Parse E-mails from Folders" run, so a batch that was just read can be reviewed
-- as a filtered e-mail list right after it finishes (and later, from the connection page).
CREATE TABLE IF NOT EXISTS parse_batches (
    id VARCHAR(36) CHARACTER SET ascii COLLATE ascii_bin NOT NULL PRIMARY KEY,
    connection_id VARCHAR(36) CHARACTER SET ascii COLLATE ascii_bin,
    folder TEXT,
    batch_size INT,
    status VARCHAR(16) NOT NULL DEFAULT 'running',
    started_at BIGINT,
    finished_at BIGINT,
    processed INT NOT NULL DEFAULT 0,
    skipped INT NOT NULL DEFAULT 0,
    errors INT NOT NULL DEFAULT 0,
    remaining INT NOT NULL DEFAULT 0
);

-- The e-mails newly saved by a batch. message_id mirrors headers.message_id's type so the e-mail
-- list's IN (SELECT ...) filter compares equal collations.
CREATE TABLE IF NOT EXISTS parse_batch_emails (
    batch_id VARCHAR(36) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    message_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    PRIMARY KEY (batch_id, message_id),
    FOREIGN KEY (batch_id) REFERENCES parse_batches(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_parse_batches_connection_started ON parse_batches(connection_id, started_at);
