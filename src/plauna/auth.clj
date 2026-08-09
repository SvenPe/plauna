(ns plauna.auth
  "Web UI authentication: password protection plus optional mTLS authentication through a trusted
   reverse proxy.

   The active password is established at startup by initialize! with the precedence:
   PLAUNA_PASSWORD environment variable > previously stored password > a freshly generated one
   (printed to the log once). The hash is persisted in the preferences table and cached in memory.

   mTLS authentication is enabled by PLAUNA_MTLS_TRUSTED_CERT_SHA256. NGINX must verify the client
   certificate and forward its escaped PEM value. A separate shared secret authenticates NGINX to
   Plauna so clients cannot gain access by forging the forwarded headers."
  (:require [clojure.string :as str]
            [plauna.database :as db]
            [taoensso.telemere :as t])
  (:import (javax.crypto SecretKeyFactory)
           (javax.crypto.spec PBEKeySpec)
           (java.io ByteArrayInputStream)
           (java.net URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.security SecureRandom MessageDigest)
           (java.security.cert CertificateFactory X509Certificate)
           (java.util Base64 HexFormat)))

(set! *warn-on-reflection* true)

(def ^:private iterations 210000)
(def ^:private key-bits 256)
(def ^:private salt-bytes 16)

(defonce ^:private ^SecureRandom rng (SecureRandom.))

;; Runtime cache of the stored password hash (a "pbkdf2$iterations$salt$hash" string).
(defonce password-hash (atom nil))

;; Runtime-only mTLS proxy configuration. The proxy secret is intentionally never persisted.
(defonce ^:private mtls-auth-config (atom nil))

(defn ^:dynamic system-env [key] (System/getenv key))

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
  (not (str/blank? (system-env "PLAUNA_PASSWORD"))))

(defn- normalize-sha256-fingerprint [fingerprint]
  (let [normalized (some-> fingerprint
                           str/trim
                           str/lower-case
                           (str/replace ":" ""))]
    (when (and normalized (re-matches #"[0-9a-f]{64}" normalized))
      normalized)))

(defn mtls-config-from-env
  "Build the mTLS proxy configuration from an environment map/function. Returns nil when no
   certificate allowlist is configured and throws for incomplete or malformed opt-in settings."
  [env]
  (let [lookup       #(if (map? env) (get env %) (env %))
        raw-certs    (lookup "PLAUNA_MTLS_TRUSTED_CERT_SHA256")
        proxy-secret (lookup "PLAUNA_MTLS_PROXY_SECRET")]
    (when-not (str/blank? raw-certs)
      (when (< (count (or proxy-secret "")) 32)
        (throw (ex-info "PLAUNA_MTLS_PROXY_SECRET must contain at least 32 characters when mTLS authentication is enabled."
                        {})))
      (let [provided     (remove str/blank? (str/split raw-certs #"[,;\s]+"))
            fingerprints (map normalize-sha256-fingerprint provided)]
        (when (or (empty? provided) (some nil? fingerprints))
          (throw (ex-info "PLAUNA_MTLS_TRUSTED_CERT_SHA256 must contain only full SHA-256 certificate fingerprints."
                          {})))
        {:proxy-secret proxy-secret
         :fingerprints (set fingerprints)}))))

(defn initialize-mtls!
  "Load the optional mTLS proxy authentication configuration. Invalid opt-in configuration fails
   closed: password authentication remains available, but forwarded certificates are not accepted."
  []
  (try
    (let [config (mtls-config-from-env system-env)]
      (reset! mtls-auth-config config)
      (when config
        (t/log! :info ["mTLS proxy authentication enabled for"
                       (count (:fingerprints config))
                       "certificate fingerprint(s)."])))
    (catch Exception e
      (reset! mtls-auth-config nil)
      (t/log! {:level :error :error e}
              "mTLS proxy authentication is disabled because its environment configuration is invalid."))))

(defn- constant-time-string= [expected actual]
  (and (string? expected)
       (string? actual)
       (MessageDigest/isEqual (.getBytes ^String expected StandardCharsets/UTF_8)
                              (.getBytes ^String actual StandardCharsets/UTF_8))))

(defn client-certificate-sha256
  "Decode an NGINX $ssl_client_escaped_cert header and return the certificate's canonical SHA-256
   fingerprint, or nil when the header does not contain one valid X.509 certificate."
  [escaped-pem]
  (when-not (str/blank? escaped-pem)
    (try
      (let [pem     (URLDecoder/decode ^String escaped-pem StandardCharsets/UTF_8)
            factory (CertificateFactory/getInstance "X.509")]
        (with-open [input (ByteArrayInputStream. (.getBytes ^String pem StandardCharsets/UTF_8))]
          (let [certificate ^X509Certificate (.generateCertificate factory input)
                digest      (.digest (MessageDigest/getInstance "SHA-256") (.getEncoded certificate))]
            (.formatHex (HexFormat/of) digest))))
      (catch Exception _ nil))))

(defn mtls-request-authorized-with-config?
  "True when a request carries the three headers supplied by the trusted NGINX configuration and
   the verified client certificate is in the configured SHA-256 allowlist."
  [config request]
  (let [headers (:headers request)]
    (boolean
     (and config
          (constant-time-string= (:proxy-secret config)
                                 (get headers "x-plauna-proxy-secret"))
          (= "SUCCESS" (get headers "x-plauna-client-verify"))
          (when-let [fingerprint (client-certificate-sha256
                                  (get headers "x-plauna-client-cert"))]
            (contains? (:fingerprints config) fingerprint))))))

(defn mtls-request-authorized?
  "True when the request was authenticated with an explicitly allowed mTLS client certificate."
  [request]
  (mtls-request-authorized-with-config? @mtls-auth-config request))

(defn initialize!
  "Establish the optional mTLS proxy authentication and web UI password at startup. Password precedence:
   1. PLAUNA_PASSWORD env var (authoritative on every boot),
   2. a previously stored password,
   3. a freshly generated password (logged once)."
  []
  (initialize-mtls!)
  (let [env-pw (system-env "PLAUNA_PASSWORD")
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
