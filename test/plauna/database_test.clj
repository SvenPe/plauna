(ns plauna.database-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async]
            [plauna.database :as db]
            [taoensso.telemere :as t]
            [plauna.files :as files]))

(t/set-min-level! :error)

(defn setup-clean-db [f]
  (swap! files/plauna-config (fn [_] {:data-folder "tmp/"}))
  (files/check-and-create-database-file)
  (db/create-db)
  (alter-var-root #'db/batch-size (fn [_] 2))
  (f)
  (files/delete-database-file))

(use-fixtures :once setup-clean-db)

(defn- ensure-category
  "Create (or reuse) a category and return its id: metadata.category is a real foreign key, so
   tests can no longer write metadata rows pointing at made-up category ids."
  [category-name]
  (or (:id (db/category-by-name category-name))
      (do (db/create-category category-name)
          (:id (db/category-by-name category-name)))))

(deftest convert-to-count-ignores-subquery-select-from
  ;; A WHERE EXISTS (SELECT ... FROM ...) subquery (e.g. the "From" e-mail filter) has its own
  ;; "SELECT ... FROM" later in the SQL string. convert-to-count must rewrite only the outer
  ;; projection; rewriting the subquery's too leaves a second "%s" with no argument to fill it,
  ;; which throws a MissingFormatArgumentException at query time.
  (let [sql-result ["SELECT headers.message_id, subject FROM headers WHERE EXISTS (SELECT 1 FROM communications WHERE communications.message_id = headers.message_id) ORDER BY headers.date DESC" "param1"]
        [count-sql & params] (db/convert-to-count sql-result :enriched-email)]
    (is (= "SELECT COUNT(headers.message_id) as count FROM headers WHERE EXISTS (SELECT 1 FROM communications WHERE communications.message_id = headers.message_id)" count-sql))
    (is (= ["param1"] params))))

(deftest save-email-batch
  (let [example {:type :parsed-email :payload {:header {:message-id "test" :date 0 :subject "Test" :in-reply-to nil :mime-type "text/plain"} :body [{:message-id "test" :mime-type "text/plain" :charset "fake" :transfer-encoding "fake" :content "Test" :sanitized-content "Test"}] :participants [{:type :sender :message-id "test" :name "fake" :address "fake" :contact-key "fake"} {:type :receiver :message-id "test" :name "fake" :address "fake" :contact-key "fake"}]}}
        to-insert (repeatedly 6 (fn [] example))
        test-channel (async/chan)
        test-publisher (async/pub test-channel :type)]
    (db/database-event-loop test-publisher)
    (doseq [test-event to-insert] (async/>!! test-channel test-event))
    (Thread/sleep 1000)
    (async/close! test-channel)))

(deftest database-event-loop-flushes-a-partial-buffer-when-input-closes
  (let [event {:type :parsed-email
               :payload {:header {:message-id "shutdown-buffer"}
                         :body [] :participants []}}
        input (async/chan)
        publisher (async/pub input :type)
        saved (atom [])]
    (with-redefs [db/save-emails-in-buffer (fn [buffer] (swap! saved conj buffer))]
      (let [worker (db/database-event-loop publisher)]
        (async/>!! input event)
        (async/close! input)
        (is (= :stopped (async/<!! worker)))
        (is (= ["shutdown-buffer"]
               (mapv #(get-in % [:headers 0 :message-id]) @saved))))))
  "Shutdown waits for JDBC work on its dedicated thread and saves the final partial batch")

(deftest enriched-email-simple
  (let [sql (db/data->sql {:entity :enriched-email :strict false})]
    (is (=  "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers LEFT JOIN metadata ON headers.message_id = metadata.message_id"
            (first sql)))))

(deftest enriched-email-simple-2
  (let [sql (db/data->sql {:entity :enriched-email :strict true})]
    (is (=  "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id"
            (first sql)))))

(deftest enriched-email-simple-3
  (let [sql (db/data->sql {:entity :enriched-email :strict true} {:where [:= :message-id "123"]})]
    (is (=  "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id WHERE headers.message_id = ?"
            (first sql)))))

(deftest enriched-email-simple-4
  (let [sql (db/data->sql {:entity :enriched-email :strict true} {:where [:and [:= :message-id "123"] [:<> :language nil] [:<> :category nil]]})]
    (is (= "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id WHERE (headers.message_id = ?) AND (metadata.language IS NOT NULL) AND (metadata.category IS NOT NULL)"
           (first sql)))))

(deftest email-folder-round-trip
  (db/save-headers [{:mime-type "text/plain" :subject "f" :message-id "folder-rt" :date 0 :in-reply-to nil}])
  (db/update-metadata-category "folder-rt" nil 1.0)
  (db/update-email-folder "folder-rt" "Archive/Projects")
  (is (= "Archive/Projects" (db/email-folder "folder-rt")) "Recorded folder is persisted and read back through metadata.folder")
  (is (nil? (db/email-folder "no-such-message")) "Unknown message has no recorded folder"))

(deftest distinct-subjects-excludes-duplicates-and-blanks
  (db/save-headers [{:mime-type "text/plain" :subject "Unique Subject A" :message-id "subj-1" :date 0 :in-reply-to nil}
                     {:mime-type "text/plain" :subject "Unique Subject A" :message-id "subj-2" :date 0 :in-reply-to nil}
                     {:mime-type "text/plain" :subject "Unique Subject B" :message-id "subj-3" :date 0 :in-reply-to nil}
                     {:mime-type "text/plain" :subject "" :message-id "subj-4" :date 0 :in-reply-to nil}
                     {:mime-type "text/plain" :subject nil :message-id "subj-5" :date 0 :in-reply-to nil}])
  (let [subjects (set (map :subject (db/distinct-subjects)))]
    (is (contains? subjects "Unique Subject A"))
    (is (contains? subjects "Unique Subject B"))
    (is (not (contains? subjects "")) "A blank subject is excluded")
    (is (not (contains? subjects nil)) "A NULL subject is excluded")))

(deftest distinct-senders-and-recipients-are-split-by-participant-type
  (db/save-headers [{:mime-type "text/plain" :subject "s" :message-id "contact-msg" :date 0 :in-reply-to nil}])
  (db/save-contacts [{:contact-key "sender-key" :name "Alice" :address "alice@example.com"}
                      {:contact-key "recipient-key" :name "Bob" :address "bob@example.com"}])
  (db/save-communications [{:message-id "contact-msg" :contact-key "sender-key" :type :sender}
                            {:message-id "contact-msg" :contact-key "recipient-key" :type :receiver}])
  (let [sender-keys (set (map :contact_key (db/distinct-senders)))
        recipient-keys (set (map :contact_key (db/distinct-recipients)))]
    (is (contains? sender-keys "sender-key"))
    (is (not (contains? sender-keys "recipient-key")) "A receiver is not listed as a sender")
    (is (contains? recipient-keys "recipient-key"))
    (is (not (contains? recipient-keys "sender-key")) "A sender is not listed as a recipient")))

(deftest distinct-subjects-scoped-by-other-filters-where
  ;; Faceted filtering: once another column filter (here, category) is active, the Subject
  ;; checklist should only offer subjects that actually occur under that filter.
  (db/save-headers [{:mime-type "text/plain" :subject "Facet Subject In Category" :message-id "facet-subj-1" :date 0 :in-reply-to nil}
                     {:mime-type "text/plain" :subject "Facet Subject Elsewhere" :message-id "facet-subj-2" :date 0 :in-reply-to nil}])
  (let [cat-id (ensure-category "facet-subject-category")]
    (db/update-metadata-category "facet-subj-1" cat-id 1.0)
    (db/update-metadata-category "facet-subj-2" nil 1.0)
    (let [scoped (set (map :subject (db/distinct-subjects [:= :metadata.category cat-id])))]
      (is (contains? scoped "Facet Subject In Category"))
      (is (not (contains? scoped "Facet Subject Elsewhere")) "A subject outside the scoping filter is excluded"))))

(deftest distinct-senders-scoped-by-other-filters-where
  (db/save-headers [{:mime-type "text/plain" :subject "s" :message-id "facet-sender-1" :date 0 :in-reply-to nil}
                     {:mime-type "text/plain" :subject "s" :message-id "facet-sender-2" :date 0 :in-reply-to nil}])
  (db/save-contacts [{:contact-key "facet-sender-key-a" :name "A" :address "a@example.com"}
                      {:contact-key "facet-sender-key-b" :name "B" :address "b@example.com"}])
  (db/save-communications [{:message-id "facet-sender-1" :contact-key "facet-sender-key-a" :type :sender}
                            {:message-id "facet-sender-2" :contact-key "facet-sender-key-b" :type :sender}])
  (let [cat-id (ensure-category "facet-sender-category")]
    (db/update-metadata-category "facet-sender-1" cat-id 1.0)
    (db/update-metadata-category "facet-sender-2" nil 1.0)
    (let [scoped (set (map :contact_key (db/distinct-senders [:= :metadata.category cat-id])))]
      (is (contains? scoped "facet-sender-key-a"))
      (is (not (contains? scoped "facet-sender-key-b")) "A sender outside the scoping filter is excluded"))))

(deftest distinct-header-categories-returns-reachable-categories
  (db/save-headers [{:mime-type "text/plain" :subject "s" :message-id "facet-cat-1" :date 0 :in-reply-to nil}
                     {:mime-type "text/plain" :subject "s" :message-id "facet-cat-2" :date 0 :in-reply-to nil}])
  (db/save-contacts [{:contact-key "facet-cat-sender" :name "A" :address "a@example.com"}])
  (db/save-communications [{:message-id "facet-cat-1" :contact-key "facet-cat-sender" :type :sender}])
  (let [cat-a (ensure-category "facet-reachable-category-a")
        cat-b (ensure-category "facet-reachable-category-b")]
    (db/update-metadata-category "facet-cat-1" cat-a 1.0)
    (db/update-metadata-category "facet-cat-2" cat-b 1.0)
    (let [scoping-where [:in :headers.message-id
                         {:select [:communications.message-id] :from [:communications]
                          :where [:= :communications.contact-key "facet-cat-sender"]}]
          reachable (set (map :category (db/distinct-header-categories scoping-where)))
          unscoped (set (map :category (db/distinct-header-categories nil)))]
      (is (= #{cat-a} reachable) "Only the category reachable through the sender-scoped where-clause is returned")
      (is (contains? unscoped cat-a))
      (is (contains? unscoped cat-b) "Without a scoping where-clause, every category is reachable"))))

(deftest parse-batch-round-trip
  (db/create-parse-batch! {:id "batch-1" :connection-id "conn-1" :folder "Old" :batch-size 100})
  (db/save-headers [{:mime-type "text/plain" :subject "in batch" :message-id "batch-mail-1" :date 0 :in-reply-to nil}
                    {:mime-type "text/plain" :subject "in batch too" :message-id "batch-mail-2" :date 0 :in-reply-to nil}])
  (db/record-parse-batch-email! "batch-1" "batch-mail-1")
  (db/record-parse-batch-email! "batch-1" "batch-mail-1")
  (db/record-parse-batch-email! "batch-1" "batch-mail-2")
  (let [running (db/parse-batch "batch-1")]
    (is (= "running" (:status running)))
    (is (some? (:started-at running)))
    (is (nil? (:finished-at running))))
  (is (= ["batch-mail-1" "batch-mail-2"] (sort (db/parse-batch-message-ids "batch-1"))) "Recording the same e-mail twice stores it once")
  (db/finish-parse-batch! "batch-1" {:processed 2 :skipped 5 :errors 1 :remaining 40})
  (let [finished (db/parse-batch "batch-1")]
    (is (= "finished" (:status finished)))
    (is (= [2 5 1 40] ((juxt :processed :skipped :errors :remaining) finished)))
    (is (some? (:finished-at finished))))
  (is (= ["batch-1"] (mapv :id (db/parse-batches-for-connection "conn-1" 10))))
  (is (empty? (db/parse-batches-for-connection "other-conn" 10)))
  ;; The e-mail list's batch filter is a subquery on parse_batch_emails.
  (is (= ["batch-mail-1" "batch-mail-2"]
         (sort (map :message-id (db/fetch-headers {:entity :enriched-email :strict false}
                                                   {:where [:in :headers.message-id {:select [:parse-batch-emails.message-id]
                                                                                     :from [:parse-batch-emails]
                                                                                     :where [:= :parse-batch-emails.batch-id "batch-1"]}]}))))))

(deftest abort-running-parse-batches-marks-only-running-runs
  (db/create-parse-batch! {:id "batch-running" :connection-id "conn-2" :folder "A" :batch-size nil})
  (db/create-parse-batch! {:id "batch-done" :connection-id "conn-2" :folder "B" :batch-size 10})
  (db/finish-parse-batch! "batch-done" {:processed 1})
  (db/abort-running-parse-batches!)
  (is (= "aborted" (:status (db/parse-batch "batch-running"))))
  (is (= "finished" (:status (db/parse-batch "batch-done")))))

(deftest metadata-keeps-a-known-connection-id
  (db/save-headers [{:mime-type "text/plain" :subject "conn" :message-id "conn-rt" :date 0 :in-reply-to nil}])
  (db/update-metadata-batch [{:message-id "conn-rt" :language "eng" :language-confidence 0.9 :category-id nil :category-confidence 0 :connection-id "conn-1"}])
  (is (= "conn-1" (:connection-id (db/fetch-metadata "conn-rt"))))
  (is (= "conn-1" (db/email-connection-id "conn-rt")))
  ;; A later save that does not know the account (mbox import, re-enrichment) must not blank it.
  (db/update-metadata-batch [{:message-id "conn-rt" :language "deu" :language-confidence 0.8 :category-id nil :category-confidence 0 :connection-id nil}])
  (let [metadata (db/fetch-metadata "conn-rt")]
    (is (= "deu" (:language metadata)) "Other columns are still updated")
    (is (= "conn-1" (:connection-id metadata)) "A NULL never overwrites a known account"))
  (db/update-email-connection "conn-rt" "conn-2")
  (is (= "conn-2" (db/email-connection-id "conn-rt")))
  (is (= "conn-2" (:connection-id (first (db/fetch-metadata-for ["conn-rt"])))) "The batch fetch exposes the account too")
  (is (nil? (db/email-connection-id "no-such-message"))))

(deftest parse-failures-are-recorded-updated-and-resolved
  (db/record-parse-failure! {:connection-id "conn-f" :folder "INBOX" :uid 501 :message-number 7 :message-id nil :subject "first" :error "Failed to load IMAP envelope"})
  (db/record-parse-failure! {:connection-id "conn-f" :folder "INBOX" :uid 501 :message-number 6 :message-id "<m-501>" :subject nil :error "again"})
  (db/record-parse-failure! {:connection-id "conn-f" :folder "Archive" :uid nil :message-number 1 :message-id nil :subject nil :error "no uid"})
  (db/record-parse-failure! {:connection-id "conn-f" :folder "Archive" :uid nil :message-number 2 :message-id nil :subject nil :error "no uid either"})
  (let [failures (db/parse-failures-for-connection "conn-f" 100)
        inbox (first (filter #(= "INBOX" (:folder %)) failures))]
    (is (= 3 (count failures)) "Two failures without UID stay separate; the same UID is one entry")
    (is (= 2 (:attempts inbox)))
    (is (= 6 (:message-number inbox)))
    (is (= "<m-501>" (:message-id inbox)) "A later attempt fills in the Message-ID")
    (is (= "first" (:subject inbox)) "A later attempt without subject keeps the known one")
    (is (= "again" (:error inbox))))
  (is (= 1 (count (db/parse-failures-for-folder "conn-f" "INBOX"))))
  (db/resolve-parse-failures! "conn-f" "INBOX" nil "<m-501>")
  (is (empty? (db/parse-failures-for-folder "conn-f" "INBOX")) "Resolving by Message-ID removes the entry")
  (let [archive (db/parse-failures-for-folder "conn-f" "Archive")]
    (db/delete-parse-failure! (:id (first archive)))
    (is (= 1 (count (db/parse-failures-for-folder "conn-f" "Archive")))))
  (db/resolve-parse-failures! "conn-f" "Archive" nil nil)
  (is (= 1 (count (db/parse-failures-for-folder "conn-f" "Archive"))) "Nothing to match on: nothing is deleted"))

(deftest training-tokens-are-cached-replaced-and-invalidated
  (db/save-headers [{:mime-type "text/plain" :subject "t1" :message-id "tok-1" :date 0 :in-reply-to nil}
                    {:mime-type "text/plain" :subject "t2" :message-id "tok-2" :date 0 :in-reply-to nil}])
  (is (= {} (db/fetch-training-tokens-for [] 1)))
  (is (= {} (db/fetch-training-tokens-for ["tok-1"] 1)))
  (db/save-training-tokens! {"tok-1" "subject:a body:b" "tok-2" "subject:c"} 1)
  (is (= {"tok-1" "subject:a body:b" "tok-2" "subject:c"} (db/fetch-training-tokens-for ["tok-1" "tok-2" "tok-missing"] 1)))
  (is (= {} (db/fetch-training-tokens-for ["tok-1" "tok-2"] 2)) "Rows of an older tokenizer version are ignored")
  (db/save-training-tokens! {"tok-1" "subject:new"} 2)
  (is (= "subject:new" (get (db/fetch-training-tokens-for ["tok-1"] 2) "tok-1")) "Saving again replaces the cached tokens")
  (is (nil? (get (db/fetch-training-tokens-for ["tok-1"] 1) "tok-1")) "The replaced row carries the new version")
  (db/delete-training-tokens! "tok-1")
  (is (= {"tok-2" "subject:c"} (db/fetch-training-tokens-for ["tok-1" "tok-2"] 1)))
  (db/delete-email-by-message-id "tok-2")
  (is (= {} (db/fetch-training-tokens-for ["tok-2"] 1)) "Deleting the e-mail removes its cached tokens"))

(deftest parse-failures-without-uid-are-deduplicated-by-message-number
  (db/record-parse-failure! {:connection-id "conn-nu" :folder "INBOX" :uid nil :message-number 42 :message-id nil :subject nil :error "store gone"})
  (db/record-parse-failure! {:connection-id "conn-nu" :folder "INBOX" :uid nil :message-number 42 :message-id nil :subject nil :error "store still gone"})
  (db/record-parse-failure! {:connection-id "conn-nu" :folder "INBOX" :uid nil :message-number 43 :message-id nil :subject nil :error "store gone"})
  (let [failures (db/parse-failures-for-folder "conn-nu" "INBOX")]
    (is (= 2 (count failures)) "The same sequence number without UID is one entry")
    (is (= 2 (:attempts (first (filter #(= 42 (:message-number %)) failures))))))
  (is (= [{:folder "INBOX" :count 2}] (db/parse-failure-counts "conn-nu")))
  (is (= 1 (count (db/parse-failures-for-connection "conn-nu" 1))) "The display query honours its limit")
  (is (= {:uids #{} :message-ids #{}} (db/parse-failure-keys "conn-nu" "INBOX")))
  (db/record-parse-failure! {:connection-id "conn-nu" :folder "INBOX" :uid 900 :message-number 44 :message-id "<k@x>" :subject nil :error "e"})
  (is (= {:uids #{900} :message-ids #{"<k@x>"}} (db/parse-failure-keys "conn-nu" "INBOX"))))
