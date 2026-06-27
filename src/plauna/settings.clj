(ns plauna.settings
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [plauna.files :as files])
  (:import [java.nio.file Files StandardCopyOption]))

(def ^:private defaults
  {:log-level                    "info"
   :language-detection-threshold 0.8
   :categorization-threshold     0.65
   :client-health-check-interval 60})

(defn- settings-path []
  (str (files/file-dir) "/settings.json"))

(defn load-settings []
  (let [f (io/file (settings-path))]
    (if (.exists f)
      (merge defaults (json/parse-string (slurp f) true))
      defaults)))

(defn save-settings! [m]
  (let [path (settings-path)
        tmp  (str path ".tmp")]
    (io/make-parents path)
    (spit tmp (json/generate-string m {:pretty true}))
    (Files/move (.toPath (io/file tmp)) (.toPath (io/file path))
                (into-array StandardCopyOption
                            [StandardCopyOption/REPLACE_EXISTING
                             StandardCopyOption/ATOMIC_MOVE]))))

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
  (save-settings! (assoc (load-settings) (keyword (name k)) (coerce k v))))

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
  (let [m (load-settings)]
    (or (:session-key m)
        (let [k (random-session-key)]
          (save-settings! (assoc m :session-key k))
          k))))

(defn migrate-from-db-values!
  "One-shot migration from DB preference strings to settings.json.
   Coerces all raw values in memory first, then writes atomically in one shot.
   Does nothing if settings.json already exists. Returns true when migration ran."
  [raw-map]
  (when-not (.exists (io/file (settings-path)))
    (let [coerced (into {} (for [[k v] raw-map :when (some? v)]
                             [(keyword (name k)) (coerce k v)]))]
      (save-settings! (merge defaults coerced))
      true)))
