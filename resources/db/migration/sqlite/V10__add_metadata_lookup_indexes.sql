-- Speed up the category/language filtering and grouping used by the statistics and training queries.
-- SQLite does not auto-index foreign-key columns, so metadata.category is otherwise unindexed.
CREATE INDEX IF NOT EXISTS idx_metadata_category ON metadata(category);
CREATE INDEX IF NOT EXISTS idx_metadata_language ON metadata(language);
