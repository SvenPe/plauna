-- MariaDB supports ALTER TABLE ADD COLUMN and ADD CONSTRAINT natively,
-- so no table-rebuild is needed (unlike the SQLite version).
ALTER TABLE connections ADD COLUMN auth_type TEXT;
ALTER TABLE connections ADD COLUMN auth_provider INT;
ALTER TABLE connections ADD CONSTRAINT fk_conn_auth_provider FOREIGN KEY (auth_provider) REFERENCES auth_providers(id);
