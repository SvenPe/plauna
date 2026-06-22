(ns plauna.auth-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [plauna.auth :as auth]))

(deftest hash-and-verify-roundtrip
  (let [hash (auth/hash-password "correct horse battery staple")]
    (is (str/starts-with? hash "pbkdf2$") "Hash is stored in the pbkdf2$... format")
    (is (true? (auth/verify-password "correct horse battery staple" hash)) "The correct password verifies")
    (is (false? (auth/verify-password "wrong password" hash)) "An incorrect password does not verify"))
  "hash-password and verify-password round-trip correctly")

(deftest each-hash-uses-a-fresh-salt
  (let [a (auth/hash-password "same-password")
        b (auth/hash-password "same-password")]
    (is (not= a b) "Hashing the same password twice yields different output (random salt)")
    (is (true? (auth/verify-password "same-password" a)))
    (is (true? (auth/verify-password "same-password" b))))
  "A random salt is used per hash")

(deftest verify-is-safe-on-garbage-input
  (is (false? (auth/verify-password "x" "not-a-valid-hash")))
  (is (false? (auth/verify-password "x" "")))
  (is (false? (auth/verify-password nil (auth/hash-password "y"))))
  "verify-password returns false rather than throwing on malformed input")

(deftest generated-passwords-are-strong-and-unique
  (let [a (auth/generate-password)
        b (auth/generate-password)]
    (is (>= (count a) 20) "Generated password is long")
    (is (not= a b) "Generated passwords are unique"))
  "generate-password produces strong, unique passwords")

(deftest verify-web-password-uses-the-cached-hash
  (with-redefs [auth/password-hash (atom (auth/hash-password "the-secret"))]
    (is (true? (auth/verify-web-password? "the-secret")))
    (is (false? (auth/verify-web-password? "nope")))
    (is (false? (auth/verify-web-password? nil))))
  "verify-web-password? checks the plaintext against the in-memory hash")
