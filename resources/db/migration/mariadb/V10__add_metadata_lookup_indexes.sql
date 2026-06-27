-- Speed up language grouping/filtering (statistics, training). metadata.category is already indexed
-- via its foreign key on MariaDB, so only the language column needs an explicit index here.
CREATE INDEX IF NOT EXISTS idx_metadata_language ON metadata(language);
