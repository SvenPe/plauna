-- One row per manual "Parse E-mails from Folders" run, so a batch that was just read can be reviewed
-- as a filtered e-mail list right after it finishes (and later, from the connection page).
CREATE TABLE parse_batches(
       id TEXT PRIMARY KEY,
       connection_id TEXT,
       folder TEXT,
       batch_size INTEGER,
       status TEXT NOT NULL DEFAULT 'running',
       started_at INTEGER,
       finished_at INTEGER,
       processed INTEGER NOT NULL DEFAULT 0,
       skipped INTEGER NOT NULL DEFAULT 0,
       errors INTEGER NOT NULL DEFAULT 0,
       remaining INTEGER NOT NULL DEFAULT 0);

-- The e-mails newly saved by a batch. Deleting a batch drops its membership rows; deleting an e-mail
-- leaves a dangling membership row that the e-mail list simply never joins to anything.
CREATE TABLE parse_batch_emails(
       batch_id TEXT NOT NULL REFERENCES parse_batches(id) ON DELETE CASCADE,
       message_id TEXT NOT NULL,
       PRIMARY KEY (batch_id, message_id));

CREATE INDEX IF NOT EXISTS idx_parse_batches_connection_started ON parse_batches(connection_id, started_at);
