-- The classification features of an e-mail (sender, domain, subject and body tokens) are expensive to
-- derive (HTML cleaning, normalization). They do not depend on category or language, so they are
-- computed once and reused by every later training run. Rows vanish with their e-mail; a re-fetched
-- body deletes the row so it is recomputed.
CREATE TABLE training_tokens(
       message_id TEXT PRIMARY KEY REFERENCES headers(message_id) ON DELETE CASCADE,
       tokens TEXT NOT NULL,
       generated_at INTEGER);
