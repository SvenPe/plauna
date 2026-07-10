-- Older versions inserted a new oauth_tokens row on every authorization while reads picked an
-- arbitrary one. Keep only the newest row per connection and make connection_id unique so
-- save-oauth-token can upsert.
DELETE FROM oauth_tokens WHERE id NOT IN (SELECT MAX(id) FROM oauth_tokens GROUP BY connection_id);
CREATE UNIQUE INDEX idx_oauth_tokens_connection_id ON oauth_tokens(connection_id);
