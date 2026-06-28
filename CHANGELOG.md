# Changelog

All notable changes to this project will be documented in this file.

## [2026-06-28.1] - 2026-06-28

### 🐛 Bug Fixes

- IMAP emails with a non-text attachment (e.g. a PDF) were stored with no body parts
  ("Email has no parseable text content"). JavaMail returns an attachment's content as
  an InputStream; storing it broke the body insert on MariaDB (the header was saved
  first, so the email survived without any body). Attachment content is now dropped at
  parse time, matching the mbox parser and the documented "attachments are not stored"
  behaviour, so the text parts are saved.

## [2026-06-28.0] - 2026-06-28

### 🔒 Security

- Bind the in-app nREPL to localhost instead of all network interfaces
- Persist the session cookie key and set HttpOnly + SameSite=Lax (mitigates CSRF)
- Reject off-site redirect targets from redirect-url/Referer (open redirect)
- Return 400/500 with a generic message instead of leaking stack traces on bad input

### 🐛 Bug Fixes

- IMAP connections always used plaintext instead of SSL
- BCC recipients were stored with the CC type
- Message rate limiter could busy-spin after the main channel closed
- OAuth refresh token was deleted on transient network errors; add HTTP timeouts
- Add graceful shutdown of IMAP connections, server and watchdog on SIGTERM
- Language detection picked an arbitrary candidate instead of the best match, and crashed on hyphenated codes (e.g. zh-cn)
- Categorizing an email with empty content no longer kills the process
- Guard against nil MIME types and nil content while parsing emails
- Plain-text bodies are no longer mangled by HTML stripping
- Decode message bodies as UTF-8 deterministically
- Empty mbox files no longer emit a spurious empty email
- IMAP moves restore monitoring even on failure; health-check tasks no longer leak; only the moved message is expunged

### ⚡ Performance

- Batch-load email listing data, eliminating per-row N+1 queries
- Run IMAP health checks on a thread pool so one slow connection cannot stall the others
- Index metadata(language) and metadata(category)
- Fix a MariaDB pagination crash on the first page (negative OFFSET)
- Use a column-preserving upsert for the SQLite metadata batch (was INSERT OR REPLACE)

### 🚜 Refactor

- Remove the unused specs namespace and clear migration reflection warnings

## [2026-03-18.0] - 2026-03-18

### 🚀 Features

- Add search functionality to the email list

### 🐛 Bug Fixes

- Server error due to changes in pagination structure
- Creating categories and moving emails fail
- *(client)* Empty folder name not recognized as default folder name
- Empty strings are treated as nil in default folder names
- Non latin texts are not normalized correctly

### 🚜 Refactor

- Logic for moving emails
- Simplify logic for incoming emails and add tests

### ⚙️ Miscellaneous Tasks

- Update dependencies and docker images

## [2026-01-18.0] - 2026-01-18

### 🚀 Features

- IMAP connections are managed in the ui instead of the config file
- Use environment variables for configuration
- Add oauth2 authentication for imap

### 🐛 Bug Fixes

- Imap parsing, add new connection, statistics page
- Client deletes access token after a non-200 response from the oauth server
- Parsing emails from IMAP folders now handles all emails
- Moving an email does not go through all connections if the connection id can be guessed
- Client falls back to INBOX if the folder to monitor is empty in the config
- *(client)* Monitored folder falls back to "Inbox" when no name is provided

### ⚙️ Miscellaneous Tasks

- *(ui)* Clean up statistics page

## [2025-09-06.0] - 2025-09-06

### 🚀 Features

- Toggle the repl on/off over the ui

### 🐛 Bug Fixes

- Imap client now properly idles and handles dead connections
- Closed folder when moving emails causes emails to get stuck in the inbox
- Text normalizer removes extra whitespaces and special characters from texts properly

## [2025-07-29.0] - 2025-07-29

### 🚀 Features

- Visiting root path on server redirects to /emails if there are e-mails to show
- Add delete functionality for e-mails in the db

### 🐛 Bug Fixes

- Creating categories without any IMAP connections returns 500
- Broken reconnection logic in the IMAP client
- [**breaking**] Email url uses base64 encoding instead of urlescaping

### 📚 Documentation

- Add screenshots from new design to docs and improve

### ⚙️ Miscellaneous Tasks

- Update dependencies for security reasons
- Update ring dependencies

## [2025-05-26.0] - 2025-05-26

### 🚀 Features

- Use IMAP copy and delete when move is not available
- Move e-mail when its category is changed by user
- *(ui)* Make ui better and mobile friendly
- *(ui)* Add the new plauna logo

### 🐛 Bug Fixes

- Change text sanitization for cleaner training texts
- Adjust text sanitization for cleaner training texts
- Moving emails no longer triggers a search through the whole folder
- *(imap client)* Wrong method call during reconnection
- Faulty partial update of connection data on reconnect
- Setting category to n/a now moves messages back to Inbox
- *(ui)* Toast messages cannot be closed anymore

### ⚙️ Miscellaneous Tasks

- Add flow-storm for better debugging experience
- Update ring dependencies

## [2025-03-22.0] - 2025-03-22

### 🚀 Features

- Health check interval for IMAP client watcher is configurable
- Show sanitized text next to the original on the email details page
- *(ui)* Add pie charts to statistics pages for better data overview
- Add optional config parameters for the email client

### 🐛 Bug Fixes

- Choose correct text content to train on when text attachments present
- Evict preferences cache after updating a value
- Throw an exception if no config file can be found during startup

### 📚 Documentation

- Fix the link to the Docker image in README
- Add 'features' and 'screenshots' subsections

### 🎨 Styling

- Remove the delete buttons from admin ui

### ⚙️ Miscellaneous Tasks

- Update JRE 23 Docker image

## [2025-02-23.0] - 2025-02-23

### 🚀 Features

- *(ui)* Remove links to half-baked features

## [2025-02-21.0] - 2025-02-21

### 🚀 Features

- *(ui)* Reorganize e-mail lists and data training
- *(ui)* Clean up and visually improve /emails
- *(ui)* Unify input styles on different pages
- *(ui)* Email details page is styled in the new fashion
- [**breaking**] Remove check for training binaries before starting imap client
- Enrich e-mails parsed from an mbox
- Rename the page "watchers" to "connections"
- Create directories on imap servers upon category creation
- Set log level from ui
- *(ui)* Async operations and errors are shown to the user as toast messages

### 🐛 Bug Fixes

- Message ids are now url encoded in the email list
- Exception when moving emails if they could not be categorized
- Save language metadata in detail view and preferences
- Display category as n/a in email views if note set
- N/a no longer listed as a language in admin
- Event loops restart when they fail
- Change wrong order of functions on main
- Compare categorization threshold with the probability correctly
- Restart all event loops after a failure or restart in messaging
- Add new languages to language preferences with the value false
- Confidence is set properly after categorization

