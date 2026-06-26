-- MariaDB baseline: creates all tables in their post-V3 form (CASCADE FKs, V2 column renames).
-- V2 and V3 are no-ops on MariaDB.
--
-- All columns used as primary/unique/foreign keys are typed VARCHAR with CHARACTER SET ascii
-- COLLATE ascii_bin so they occupy exactly one byte per character under utf8mb4 databases.
-- RFC 5321 / RFC 2822 require message IDs and MIME types to be ASCII, so this is safe.
-- 998 chars * 1 byte = 998 bytes, well within InnoDB's 3072-byte key limit.
--
-- bodies adds a STORED generated column content_md5 = MD5(content) so the unique constraint
-- can mirror SQLite's UNIQUE(mime_type, message_id, content): multiple body parts with the
-- same MIME type but different content are preserved, and NULL content is treated as unique
-- (MariaDB NULL != NULL in unique indexes).

CREATE TABLE IF NOT EXISTS headers (
    message_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL PRIMARY KEY,
    in_reply_to TEXT,
    mime_type TEXT,
    subject TEXT,
    date BIGINT
);

CREATE INDEX idx_headers_message_id ON headers(message_id);

CREATE TABLE IF NOT EXISTS contacts (
    contact_key VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL PRIMARY KEY,
    name TEXT,
    address TEXT
);

CREATE TABLE IF NOT EXISTS categories (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) UNIQUE
);

CREATE TABLE IF NOT EXISTS category_training_preferences (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    language VARCHAR(20) NOT NULL UNIQUE,
    use_in_training BOOLEAN
);

CREATE TABLE IF NOT EXISTS bodies (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    content LONGTEXT,
    mime_type VARCHAR(100) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    charset VARCHAR(50),
    transfer_encoding VARCHAR(50),
    filename TEXT,
    content_disposition TEXT,
    message_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    content_md5 CHAR(32) AS (MD5(content)) STORED,
    FOREIGN KEY (message_id) REFERENCES headers(message_id) ON DELETE CASCADE,
    UNIQUE KEY unique_body (mime_type, message_id, content_md5)
);

CREATE INDEX idx_bodies_message_id ON bodies(message_id);

CREATE TABLE IF NOT EXISTS communications (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    message_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin,
    contact_key VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin,
    type VARCHAR(50) CHARACTER SET ascii COLLATE ascii_bin,
    FOREIGN KEY (message_id) REFERENCES headers(message_id) ON DELETE CASCADE,
    FOREIGN KEY (contact_key) REFERENCES contacts(contact_key) ON DELETE CASCADE,
    UNIQUE KEY unique_comm (message_id, contact_key, type)
);

CREATE TABLE IF NOT EXISTS metadata (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    message_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin UNIQUE,
    language VARCHAR(20),
    language_modified BIGINT,
    language_confidence DOUBLE,
    category INT,
    category_modified BIGINT,
    category_confidence DOUBLE,
    FOREIGN KEY (message_id) REFERENCES headers(message_id) ON DELETE CASCADE,
    FOREIGN KEY (category) REFERENCES categories(id)
);

CREATE INDEX idx_metadata_message_id ON metadata(message_id);

CREATE TABLE IF NOT EXISTS preferences (
    preference VARCHAR(100) CHARACTER SET ascii COLLATE ascii_bin UNIQUE,
    value TEXT
);
