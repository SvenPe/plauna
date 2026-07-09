# Changelog

All notable changes to this project will be documented in this file.

## [2026-07-09.0] - 2026-07-09

### 🚀 Features

- Automatically catch up on mail missed while a connection was offline. Live
  monitoring uses IMAP IDLE, which only delivers messages that arrive while
  connected, so a dropped connection or a container restart previously left a
  permanent gap (mail that arrived during the outage never appeared, even after
  reconnecting) until a manual folder parse. Now, whenever a connection is
  (re)established - at startup, on a manual reconnect, or when the periodic
  health check restores a dropped connection - Plauna re-reads the most recent
  messages in the monitored folder and saves/categorizes anything that was
  missed. Already-stored e-mails are skipped, so nothing is duplicated. The
  scan is bounded to the most recent messages; a larger gap is still
  recoverable with the manual folder Parse control.

## [2026-07-08.3] - 2026-07-08

### 🚀 Features

- Each Subject/From/To/Category filter checklist now only lists values still
  reachable given every other active filter (e.g. once a category is picked, the
  From checklist only shows senders who actually have mail in that category),
  matching Excel's own AutoFilter behavior. The per-row category reassignment
  dropdown is unaffected and always offers every category.

### 🐛 Bug Fixes

- Fix long subjects and sender names/addresses overflowing the filter dropdowns:
  widened the popovers and switched to proper CSS ellipsis truncation, with the
  full value still available as a hover tooltip.

## [2026-07-08.2] - 2026-07-08

### 🐛 Bug Fixes

- Fix the "(Select All)" checkbox in a Subject/From/To/Category filter dropdown
  immediately resetting the filter instead of letting you pick specific values:
  it now only toggles the checkboxes and no longer submits by itself.
- Fix HTTP 414 (URI Too Long) still occurring when actually narrowing a
  Subject/From/To filter (e.g. unchecking a couple of senders out of hundreds
  still left most of them checked). Each checklist now always submits
  whichever side - checked or unchecked - is smaller, capping the query
  string at roughly half the list regardless of which few values you're
  including or excluding.

## [2026-07-08.1] - 2026-07-08

### 🐛 Bug Fixes

- Fix HTTP 414 (URI Too Long) when using the Subject/From/To filter checklists on
  a mailbox with many distinct senders or recipients: a fully-checked checklist
  (the default, unfiltered state) no longer submits every individual value.
- Checking or unchecking a box in the Subject/From/To/Category filter dropdowns
  now applies the filter immediately, instead of requiring a scroll down to the
  Apply button.

## [2026-07-08.0] - 2026-07-08

### 🚀 Features

- Add Excel-style checklist filters to the e-mail list's "Subject", "From", and
  "To" column headers, matching the existing "Category" filter: check one or
  more distinct values and click Apply. Each dropdown includes a search box to
  narrow a long list of values, since unlike categories these can number in
  the hundreds.
- The "Search Text" field now matches e-mail body content instead of the
  subject, since Subject filtering moved to its own column header filter.

## [2026-07-07.4] - 2026-07-07

### 🚀 Features

- Add an Excel-style filter to the e-mail list's "Category" column header: click it
  to check one or more categories (plus "n/a" for uncategorized e-mails) and click
  Apply. Every category is checked by default; a "(Select All)" toggle flips them
  all at once, and the selection is preserved across pagination.
- Extend category color-coding to the whole row: each e-mail list row is now tinted
  with its category's color at 20% opacity, in addition to the dropdown's left
  border added previously.

## [2026-07-07.3] - 2026-07-07

### 🚀 Features

- Add color-coding for e-mail categories. Set a category's color with a color picker
  in Admin > Categories; the category dropdown on the e-mail list and on an e-mail's
  details page shows a left-border accent matching the selected category's color.

## [2026-07-07.2] - 2026-07-07

### 🐛 Bug Fixes

- Fix the Subject filter added in 2026-07-07.1: it rendered full-width inside the
  table header, which distorted the column widths and hid other columns. Removed it
  and restored the normal-sized "Search Text" field in the filter card instead.

## [2026-07-07.1] - 2026-07-07

### 🚀 Features

- Add a calendar date-range picker for the e-mail list's date filter, replacing the
  two separate "Date From"/"Date To" boxes. Vendored locally rather than loaded from
  a CDN, so it keeps working without internet access.
- Add a "Subject" filter directly in the e-mail list's header row, and a new "From"
  filter that matches a sender's name or address.
- Make the e-mail list's page size a free-form field (previously a fixed 10/20/30
  dropdown), moved to the bottom of the list.

### 🐛 Bug Fixes

- Fix a crash (500) when using the new "From" filter: the pagination count query
  rewrote SQL text with a regex that also matched the filter's own subquery,
  producing an unfillable placeholder.
- Escape `%` and `_` in Subject/From search text so they match literally instead of
  acting as SQL LIKE wildcards.
- Match e-mails whose sender participant type was stored as the legacy `:sender`
  spelling (with the leading colon), not just `sender`.
- Skip loading full e-mail body content for the list view, which never renders it -
  relevant now that the page size can go as high as 500.

## [2026-07-07.0] - 2026-07-07

### 🐛 Bug Fixes

- Fix the IMAP message listener being recreated (instead of reused) on every
  monitoring restart. Since `stop-monitoring` only ever removed the listener
  stored on the connection, each restart (e.g. after moving an email into the
  monitored folder) left the old listener attached and added a new one, so
  incoming emails were processed once per stacked listener.
- Restart the periodic health check after a category move that pauses and
  resumes monitoring on the connection's own folder. It was being cancelled
  but never rescheduled, so the connection silently lost automatic reconnect
  after the first such move.
- Fix connections read from the config file on first boot never actually
  connecting: the code looked up the freshly generated connection id in the
  wrong map, always got `nil`, and only succeeded on the next restart once the
  connection existed in the database.
- Fix "Categorize Fresh Data" always clearing the category (and never setting
  one) because it read a `:sanitized-content` field that only exists on
  emails prepared for display, not on raw body parts from the database.
- Fix a broken redirect URL after deleting an auth provider (the status code
  ended up appended inside the URL string instead of being passed to the
  redirect).
- Close the output stream used to write a freshly trained language model
  instead of leaking the file descriptor.
- Fix a rate limiter that could freeze the whole event pipeline (e.g. on a
  second mbox upload) if an earlier upload or "detect languages" run left its
  limiter's token bucket abandoned and blocked on.

## [2026-06-28.3] - 2026-06-28

### 🐛 Bug Fixes

- Fix IMAP IDLE monitoring for SSL connections, which broke with "Folder is not using
  SocketChannels" during health checks. SSL is enabled via mail.imap.ssl.enable on the
  "imap" store; switching SSL configs to the "imaps" store (in 2026-06-28.0) moved
  property resolution to the mail.imaps.* prefix, so mail.imap.usesocketchannels no
  longer applied and IdleManager could not watch the folder.

## [2026-06-28.2] - 2026-06-28

### 🚀 Features

- Add a "Re-fetch from server" button on the e-mail details page. It re-reads the
  message from the IMAP server by Message-ID and fills in data that could not be
  extracted earlier (body parts, and the language when it was never detected),
  without clobbering content or a language the user set manually.

## [2026-06-28.1] - 2026-06-28

### 🐛 Bug Fixes

- IMAP emails with a non-text attachment (e.g. a PDF) were stored with no body parts
  ("Email has no parseable text content"). JavaMail returns an attachment's content as
  an InputStream; storing it broke the body insert on MariaDB (the header was saved
  first, so the email survived without any body). Attachment content is now dropped at
  parse time, matching the mbox parser and the documented "attachments are not stored"
  behaviour, so the text parts are saved.
- Live IMAP parsing no longer fails for a text part with no charset parameter (e.g.
  "Content-Type: text/plain"); the charset helper falls back instead of throwing an NPE.

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

