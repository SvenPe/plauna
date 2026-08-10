(ns plauna.settings
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [plauna.files :as files])
  (:import [java.nio.file AtomicMoveNotSupportedException Files StandardCopyOption]
           [java.time ZoneId]))

(def ^:private settings-lock (Object.))

(def ^:private defaults
  {:web-login-name               "root"
   :log-level                    "info"
   :language-detection-threshold 0.8
   :categorization-threshold     0.65
   :categorization-algorithm     "naive-bayes"
   :client-health-check-interval 60
   :automatic-training-time      "02:00"
   :time-zone                    (.getId (ZoneId/systemDefault))})

(defn- settings-path []
  (str (files/file-dir) "/settings.json"))

(defn load-settings []
  (let [f (io/file (settings-path))]
    (if (.exists f)
      (merge defaults (json/parse-string (slurp f) true))
      defaults)))

(defn save-settings! [m]
  (locking settings-lock
    (let [path (settings-path)
          tmp  (str path ".tmp")
          source (.toPath (io/file tmp))
          target (.toPath (io/file path))]
      (io/make-parents path)
      (spit tmp (json/generate-string m {:pretty true}))
      (try
        (Files/move source target
                    (into-array StandardCopyOption
                                [StandardCopyOption/REPLACE_EXISTING
                                 StandardCopyOption/ATOMIC_MOVE]))
        (catch AtomicMoveNotSupportedException _
          (Files/move source target
                      (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING])))))))

(defn fetch-setting [k]
  (get (load-settings) (keyword (name k))))

(defn- coerce [k v]
  (let [d (get defaults (keyword (name k)))]
    (cond
      (instance? Double d) (Double/parseDouble (str v))
      (instance? Long d)   (Long/parseLong (str v))
      ;; Strip leading ":" from keywords serialised by Selmer (e.g. ":info" → "info").
      :else (let [s (str v)] (cond-> s (.startsWith s ":") (.substring 1))))))

(defn update-setting! [k v]
  ;; Loading and saving must be one critical section: the training scheduler can update its last-run
  ;; timestamp while an administrator saves preferences, and neither update may overwrite the other.
  (locking settings-lock
    (save-settings! (assoc (load-settings) (keyword (name k)) (coerce k v)))))

(defn update-settings!
  "Atomically merge several already-validated values into settings.json. Used for security settings
   that must not be observed or persisted in a partially updated state."
  [updates]
  (locking settings-lock
    (save-settings! (merge (load-settings) updates))))

(defn- random-session-key
  "A 16-character (16-byte) random string, suitable as an AES-128 key for the session cookie store."
  []
  (let [chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        rng   (java.security.SecureRandom.)]
    (apply str (repeatedly 16 #(nth chars (.nextInt rng (count chars)))))))

(defn session-key
  "Persistent secret key for the session cookie store. Generated and stored in settings.json on first
   use so that sessions survive restarts (a fresh random key per boot would log everyone out)."
  []
  (locking settings-lock
    (let [m (load-settings)]
      (or (:session-key m)
          (let [k (random-session-key)]
            (save-settings! (assoc m :session-key k))
            k)))))

(defn migrate-from-db-values!
  "One-shot migration from DB preference strings to settings.json.
   Coerces all raw values in memory first, then writes atomically in one shot.
   Does nothing if settings.json already exists. Returns true when migration ran."
  [raw-map]
  (locking settings-lock
    (when-not (.exists (io/file (settings-path)))
      (let [coerced (into {} (for [[k v] raw-map :when (some? v)]
                               [(keyword (name k)) (coerce k v)]))]
        (save-settings! (merge defaults coerced))
        true))))
