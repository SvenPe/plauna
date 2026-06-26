-- connection_id uses the same charset/collation as connections.id so the FK is valid.
CREATE TABLE IF NOT EXISTS oauth_tokens (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    connection_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    access_token TEXT NOT NULL,
    expires_in INT,
    scope TEXT,
    token_type TEXT,
    refresh_token TEXT,
    FOREIGN KEY (connection_id) REFERENCES connections(id) ON DELETE CASCADE
);
