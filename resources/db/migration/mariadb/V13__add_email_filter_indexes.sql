-- Drive date-range filtering and the date-descending e-mail list from headers instead of scanning
-- the complete mailbox. MariaDB can traverse this index in either direction.
CREATE INDEX IF NOT EXISTS idx_headers_date ON headers(date);

-- Resolve sender/recipient selections without scanning the complete communications table. The
-- existing UNIQUE(message_id, contact_key, type) index remains useful for correlated date-first
-- EXISTS probes; this order serves the non-date-filtered IN queries and filter dropdowns.
CREATE INDEX IF NOT EXISTS idx_communications_type_contact_message
    ON communications(type, contact_key, message_id);
