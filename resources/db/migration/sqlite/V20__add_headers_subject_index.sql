-- The Subject checklist of the e-mail list selects the first 500 DISTINCT subjects in alphabetical
-- order on every page load. Without an index that is a full scan plus a sort of every subject in the
-- mailbox; with it SQLite walks the index in order and stops after 500 values. (MariaDB stores subjects
-- as TEXT, where only a prefix index is possible and cannot serve the ordering, so it gets none.)
CREATE INDEX IF NOT EXISTS idx_headers_subject ON headers(subject);
