-- How many of a run's skipped messages were already stored from ANOTHER folder (duplicates), as
-- opposed to e-mails saved by an earlier run of the same folder that are still waiting to be moved.
ALTER TABLE parse_batches ADD COLUMN skipped_elsewhere INTEGER NOT NULL DEFAULT 0;
