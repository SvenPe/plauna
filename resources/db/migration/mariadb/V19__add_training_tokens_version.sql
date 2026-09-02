-- Version of the tokenizer that produced the cached tokens. Raising plauna.analysis/training-tokens-version
-- invalidates every cached row at once instead of relying on manual deletes.
ALTER TABLE training_tokens ADD COLUMN version INT NOT NULL DEFAULT 1;
