-- The classification features of an e-mail (sender, domain, subject and body tokens) are expensive to
-- derive (HTML cleaning, normalization). They do not depend on category or language, so they are
-- computed once and reused by every later training run. Rows vanish with their e-mail; a re-fetched
-- body deletes the row so it is recomputed.
CREATE TABLE IF NOT EXISTS training_tokens (
    message_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL PRIMARY KEY,
    tokens MEDIUMTEXT NOT NULL,
    generated_at BIGINT,
    FOREIGN KEY (message_id) REFERENCES headers(message_id) ON DELETE CASCADE
);
