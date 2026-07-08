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

