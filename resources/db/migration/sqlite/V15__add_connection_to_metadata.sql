-- Remember which IMAP account an e-mail belongs to, so a later move goes straight to that account
-- instead of guessing from the recipient address and trying every active connection.
ALTER TABLE metadata ADD COLUMN connection_id TEXT;

-- E-mails read by a folder parse run are known to belong to that run's account.
UPDATE metadata
   SET connection_id = (SELECT pb.connection_id
                          FROM parse_batch_emails pbe
                          JOIN parse_batches pb ON pb.id = pbe.batch_id
                         WHERE pbe.message_id = metadata.message_id
                         LIMIT 1)
 WHERE connection_id IS NULL;

-- With exactly one configured account there is nothing to guess: every remaining e-mail can only be
-- looked for there (which is also what the connection loop did before).
UPDATE metadata
   SET connection_id = (SELECT id FROM connections)
 WHERE connection_id IS NULL
   AND (SELECT COUNT(*) FROM connections) = 1;
