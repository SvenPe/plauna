-- Older versions inserted a new oauth_tokens row on every authorization while reads picked an
-- arbitrary one. Keep only the newest row per connection and make connection_id unique so
-- save-oauth-token can upsert. (Self-join DELETE instead of a subquery: MariaDB/MySQL forbid
-- selecting from the table being deleted from in a subquery.)
DELETE t1 FROM oauth_tokens t1 JOIN oauth_tokens t2 ON t1.connection_id = t2.connection_id AND t1.id < t2.id;
ALTER TABLE oauth_tokens ADD CONSTRAINT uq_oauth_tokens_connection_id UNIQUE (connection_id);
