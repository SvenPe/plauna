-- VARCHAR(998) ascii_bin matches the charset of references from oauth_tokens.connection_id.
CREATE TABLE IF NOT EXISTS connections (
    id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL PRIMARY KEY,
    host TEXT NOT NULL,
    user TEXT NOT NULL,
    secret TEXT NOT NULL,
    folder TEXT NOT NULL,
    debug BOOLEAN DEFAULT 0,
    port INT,
    security TEXT NOT NULL,
    check_ssl_certs BOOLEAN DEFAULT 1
);
