(ns plauna.files
  (:require [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.core.async :as async]
            [clojure.string :as string]
            [taoensso.telemere :as t]
            [plauna.messaging :as messaging]
            [plauna.core.events :as events])
  (:import [java.io File]
           [java.nio.file AtomicMoveNotSupportedException CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(set! *warn-on-reflection* true)

(def database-file "email.db")

(def plauna-config (atom nil))

(defn expand-home [^String s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn config [] @plauna-config)

(defn file-dir []
  (if-let [data-location (:data-folder (config))]
    (let [configured-location (expand-home data-location)]
      configured-location)
    (expand-home "~/.local/state/plauna")))

(defn check-and-create-database-file []
  (let [db-file (io/file (file-dir) database-file)]
    (if (.exists db-file)
      nil
      (do (.mkdirs (io/file (file-dir)))
          (.createNewFile db-file)))))

(defn delete-database-file []
  (let [db-file (io/file (file-dir) database-file)]
    (if (.exists db-file)
      (do (io/delete-file db-file)
          (io/delete-file (str db-file "-shm") true)
          (io/delete-file (str db-file "-wal") true))
      nil)))

(defn path-to-db-file []
  (str (io/file (file-dir) database-file)))

(defn training-file [language]
  (let [file (io/file (file-dir) (str "train-" language ".train"))]
    (if (.exists file)
      file
      (do (.createNewFile file)
          file))))

(defn files-with-type [type]
  (let [type-string (type {:model ".bin" :train ".train"})]
    (->> (filter #(and (.isFile ^File %)
                       (.endsWith (.getName ^File %) type-string)
                       (.startsWith (.getName ^File %) "train"))
                 (file-seq (clojure.java.io/file (file-dir))))
         (map (fn [f] (when (.isFile ^File f)
                        {:file     f
                         :language (subs (. ^File f getName) 6 9)}))))))

(defn training-files [] (files-with-type :train))

(defn model-files [] (files-with-type :model))

(defn- model-file-for-write [^String language model]
  (io/file (file-dir) (str "train-" language "-" model ".bin")))

(defn model-file
  "Return a model file. The two-argument form keeps Naive Bayes and MaxEnt side by side. For Naive
   Bayes it transparently reads the legacy train-<lang>.bin name until that model is retrained."
  ([^String language]
   ^File (io/file (file-dir) (str "train-" language ".bin")))
  ([^String language model]
   (let [specific (model-file-for-write language model)
         legacy (model-file language)]
     ^File (if (and (= "naive-bayes" model)
                    (not (.exists ^File specific))
                    (.exists ^File legacy))
             legacy
             specific))))

(defn write-model-file-atomically!
  "Write a model to a temporary sibling file and publish it only after writing completed successfully.
   Readers therefore see either the previous complete model or the new complete model, never a partially
   serialized file. write-fn receives the temporary file's output stream."
  ([language write-fn]
   (write-model-file-atomically! language nil write-fn))
  ([language model write-fn]
  (let [^File target-file (if model
                            (model-file-for-write language model)
                            (model-file language))]
    (io/make-parents target-file)
    (let [target-path (.toPath target-file)
          parent-path (.getParent target-path)
          prefix (str "." (.getFileName target-path) "-")
          temp-path (Files/createTempFile parent-path prefix ".tmp" (make-array FileAttribute 0))]
      (try
        (with-open [os (io/output-stream (.toFile temp-path))]
          (write-fn os))
        (try
          (Files/move temp-path target-path
                      (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                              StandardCopyOption/REPLACE_EXISTING]))
          (catch AtomicMoveNotSupportedException _
            ;; A completed sibling file is still safer than writing directly to the live model. Most
            ;; local filesystems implement this replacement as a rename even without the explicit flag.
            (t/log! :warn ["Atomic file moves are not supported for" target-path
                           "- replacing the completed model with a regular move."])
            (Files/move temp-path target-path
                        (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))
        target-file
        (finally
          (Files/deleteIfExists temp-path)))))))

(defn delete-files-with-type [type]
  (case type
    :model (doseq [file (model-files)] (io/delete-file (:file file)))
    :train (doseq [file (training-files)] (io/delete-file (:file file)))))

(defn write-to-training-file
  [language data]
  (spit (training-file language) data :append true))

(defn email-start? [line]
  (and
   (not (nil? line))
   (string/starts-with? line "From ")))

(defn read-mail-lines
  [fn sq acc]
  (loop [fn fn sq sq acc acc]
    (let [line (first sq)]
      (if (and (email-start? line) (not (nil? (peek acc))))
        (do
          (fn acc)
          (recur fn (rest sq) [line "\r\n"]))
        (if (nil? line)
          (do
            ;; Only flush a final email if we actually accumulated one; an empty/whitespace mbox would
            ;; otherwise emit a spurious empty "email".
            (when (peek acc) (fn acc))
            nil)
          (recur fn (rest sq) (conj acc line "\r\n")))))))

(defn read-emails-from-mbox
  "Reads the e-mails from an mbox (as input channel) and puts them in a :received-email event as byte arrays.

  Currently always adds the option :enrich"
  [mbox-is channel]
  (t/log! :info ["Starting to read from mbox"])
  (with-open [rdr (clojure.java.io/reader mbox-is)]
    (let [limiter (messaging/channel-limiter :parsed-enrichable-email)]
      (try
        (read-mail-lines
         (fn [email-string]
           (async/>!! (:bucket limiter) :token)
           (async/>!! channel
                      ((comp
                        (fn [mail-string] (events/create-event :received-email  mail-string {:enrich true}))
                        #(.getBytes ^String %)
                        #(apply str %)) email-string)))
         (line-seq rdr)
         [])
        (finally (messaging/close-limiter! limiter)))))
  (t/log! :info ["Finished reading mbox."]))

(defn file-exists? [path] (.exists ^File (io/file path)))

(defn config-from-file [path] (if (file-exists? path)
                                (edn/read-string (slurp path))
                                (throw (t/error! (ex-info "Provided config file at does not exist. Exiting application." {:path path})))))

(defn default-config [] {:data-folder "/var/lib/plauna/"
                         :server {:port 8080}})

(def env-var-key-pairs [["--config-file" "CONFIG_FILE"] ["--data-folder" "DATA_FOLDER"] ["--server-port" "SERVER_PORT"]])

(defmulti parse-cli-arg (fn [arg] (first arg)))
(defmethod parse-cli-arg "--config-file" [arg-pair]  {:config-file (second arg-pair)})
(defmethod parse-cli-arg "--data-folder" [arg-pair] {:data-folder (second arg-pair)})
(defmethod parse-cli-arg "--server-port" [arg-pair]
  (if-let [port (try (some-> (second arg-pair) Integer/parseInt)
                     (catch NumberFormatException _ nil))]
    {:server {:port port}}
    (do (t/log! :warn ["Ignoring --server-port: expected a numeric port but got" (pr-str (second arg-pair))])
        nil)))
(defmethod parse-cli-arg :default [arg-pair]
  (t/log! :info ["Received non Plauna specific argument" arg-pair "- Doing nothing."])
  nil)

(defn partition-cli-args [args]
  (when (odd? (count args))
    (t/log! :warn ["Ignoring trailing CLI argument with no value:" (last args)]))
  (partition 2 args))

(defn aggregate-config-map
  ([acc val] (conj acc val))
  ([val] val))

(defn ^:dynamic system-env [key] (System/getenv key))

(def env-key-pair-transformation
  (comp (map (fn [pair] (if-let [val (system-env (second pair))] [(first pair) val] nil)))
        (filter some?)
        (map parse-cli-arg)))

(defn- deep-merge
  "Recursively merge configuration maps so a high-priority nested value such as server.port does
   not discard unrelated keys from the lower-priority source. Later maps win."
  [& maps]
  (letfn [(merge-value [left right]
            (if (and (map? left) (map? right))
              (merge-with merge-value left right)
              right))]
    (reduce #(merge-with merge-value %1 %2) {} maps)))

(defn parse-config-from-cli-arguments [cli-args]
  (let [parsed-config (reduce (fn [acc val] (conj acc (parse-cli-arg val))) {} (partition-cli-args cli-args))
        env-config (transduce env-key-pair-transformation aggregate-config-map {} env-var-key-pairs)
        config-path (or (:config-file parsed-config) (:config-file env-config))
        file-config (if config-path (config-from-file config-path) {})]
    ;; Documented precedence: explicit CLI > environment > config file > built-in defaults.
    (reset! plauna-config
            (deep-merge (default-config)
                        file-config
                        (dissoc env-config :config-file)
                        (dissoc parsed-config :config-file)))))
