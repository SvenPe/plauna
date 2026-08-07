-- MariaDB/InnoDB word index for the e-mail body search. Creating the first FULLTEXT index may
-- rebuild a large existing bodies table once; startup runs Flyway before IMAP monitoring begins.
CREATE FULLTEXT INDEX IF NOT EXISTS idx_bodies_content_fulltext ON bodies(content);
