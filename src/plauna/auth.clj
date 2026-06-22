(ns plauna.auth
  "Web UI password protection: PBKDF2 password hashing plus the runtime password state.

   The active password is established at startup by initialize! with the precedence:
   PLAUNA_PASSWORD environment variable > previously stored password > a freshly generated one
   (printed to the log once). The hash is persisted in the preferences table and cached in memory."
  (:require [clojure.string :as str]
            [plauna.database :as db]
            [taoensso.telemere :as t])
  (:import (javax.crypto SecretKeyFactory)
           (javax.crypto.spec PBEKeySpec)
           (java.security SecureRandom MessageDigest)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def ^:private iterations 210000)
(def ^:private key-bits 256)
(def ^:private salt-bytes 16)

(defonce ^:private ^SecureRandom rng (SecureRandom.))

;; Runtime cache of the stored password hash (a "pbkdf2$iterations$salt$hash" string).
(defonce password-hash (atom nil))

(defn- b64 ^String [^bytes bs] (.encodeToString (Base64/getEncoder) bs))
(defn- unb64 ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- pbkdf2 ^bytes [^String password ^bytes salt ^long iters ^long bits]
  (let [spec (PBEKeySpec. (.toCharArray password) salt iters bits)
        skf (SecretKeyFactory/getInstance "PBKDF2WithHmacSHA256")]
    (.getEncoded (.generateSecret skf spec))))

(defn hash-password
  "Hash a plaintext password with a fresh random salt. Returns 'pbkdf2$<iterations>$<salt>$<hash>'."
  [^String plaintext]
  (let [salt (byte-array salt-bytes)]
    (.nextBytes rng salt)
    (str "pbkdf2$" iterations "$" (b64 salt) "$" (b64 (pbkdf2 plaintext salt iterations key-bits)))))

(defn verify-password
  "Constant-time check of a plaintext password against a stored 'pbkdf2$...' hash."
  [^String plaintext ^String stored]
  (try
    (let [[algo iters salt-b64 hash-b64] (str/split stored #"\$")]
      (and (= algo "pbkdf2")
           (some? plaintext)
           (let [expected (unb64 hash-b64)
                 actual (pbkdf2 plaintext (unb64 salt-b64) (Long/parseLong iters) (* 8 (alength expected)))]
             (MessageDigest/isEqual expected actual))))
    (catch Exception _ false)))

(defn generate-password
  "Generate a strong random password (URL-safe, ~32 characters)."
  []
  (let [bs (byte-array 24)]
    (.nextBytes rng bs)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs)))

(defn set-password!
  "Hash and persist a new web UI password, updating the in-memory cache."
  [^String plaintext]
  (let [h (hash-password plaintext)]
    (reset! password-hash h)
    (db/update-preference :web-password-hash h)))

(defn verify-web-password?
  "True if plaintext matches the active web UI password."
  [plaintext]
  (let [h @password-hash]
    (and (some? h) (verify-password plaintext h))))

(defn password-from-env-var?
  "True if the password is being supplied via the PLAUNA_PASSWORD environment variable."
  []
  (not (str/blank? (System/getenv "PLAUNA_PASSWORD"))))

(defn initialize!
  "Establish the web UI password at startup. Precedence:
   1. PLAUNA_PASSWORD env var (authoritative on every boot),
   2. a previously stored password,
   3. a freshly generated password (logged once)."
  []
  (let [env-pw (System/getenv "PLAUNA_PASSWORD")
        stored (db/fetch-preference :web-password-hash)]
    (cond
      (not (str/blank? env-pw))
      (do (set-password! env-pw)
          (t/log! :info "Web UI password set from the PLAUNA_PASSWORD environment variable."))

      (some? stored)
      (reset! password-hash stored)

      :else
      (let [pw (generate-password)]
        (set-password! pw)
        (t/log! :warn (str "================================================================\n"
                           "No web UI password configured. A temporary password was generated:\n\n"
                           "    " pw "\n\n"
                           "Log in with it and change it under Administration > Change Password,\n"
                           "or set the PLAUNA_PASSWORD environment variable.\n"
                           "================================================================"))))))
