(ns plauna.files-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [plauna.files :as files]
            [clojure.core.async :refer [<!! chan close!]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn resource->is [resource-path]
  (io/input-stream (io/resource resource-path)))

(deftest model-file-is-published-only-after-a-successful-write
  (let [temp-dir (.toFile (Files/createTempDirectory "plauna-model-test-" (make-array FileAttribute 0)))]
    (try
      (with-redefs [files/file-dir (fn [] (.getAbsolutePath temp-dir))]
        (let [target (files/model-file "deu")]
          (spit target "old model")
          (files/write-model-file-atomically!
           "deu"
           (fn [os]
             (.write os (.getBytes "new model"))
             (is (= "old model" (slurp target))
                 "Readers keep seeing the complete old model while the replacement is written")))
          (is (= "new model" (slurp target))))
        (let [target (files/model-file "eng")]
          (spit target "working model")
          (is (thrown-with-msg? Exception #"serialization failed"
                                (files/write-model-file-atomically!
                                 "eng"
                                 (fn [os]
                                   (.write os (.getBytes "partial model"))
                                   (throw (ex-info "serialization failed" {}))))))
          (is (= "working model" (slurp target))
              "A failed serialization leaves the previous model untouched")))
      (finally
        (doseq [file (reverse (file-seq temp-dir))]
          (io/delete-file file true)))))
  "Training never exposes a partially written model to categorization")

(deftest read-single-item-mbox
  (let [test-chan (chan 20)]
    (files/read-emails-from-mbox (resource->is "test/email_corpus/test-email-1.mbox") test-chan)
    (close! test-chan)
    (loop [result (<!! test-chan)
           results []]
      (if (nil? result)
        (is (= 1 (count results)))
        (recur (<!! test-chan) (conj results result))))))

(deftest read-larger-mbox
  (let [test-chan (chan 20)]
    (files/read-emails-from-mbox (resource->is  "test/email_corpus/weird-mbox.mbox") test-chan)
    (close! test-chan)
    (loop [result (<!! test-chan)
           results []]
      (if (nil? result)
        (is (= 17 (count results)))
        (recur (<!! test-chan) (conj results result))))))

(deftest parse-config-with-explicit-config
  "If --config-file argument is given, it should take precedence over anything else.
   The default values are used for any missing fields in the config file."
  (let [config-file (files/parse-config-from-cli-arguments ["--config-file" "./resources/test/test.edn"
                                                            "--server-port" "6060"])]
    (is (and (= 8080 (-> config-file :server :port)) (= "./tmp/" (:data-folder config-file))))))

(deftest parse-config-with-arguments
  "If --data-folder and --server-port arguments are given, they must be used."
  (let [config-file (files/parse-config-from-cli-arguments ["--server-port" "8081" "--data-folder" "/dev/null"])]
    (is (and (= 8081 (-> config-file :server :port)) (= "/dev/null" (:data-folder config-file))))))

(deftest parse-config-default
  "If no arguments are given, use the default config"
  (let [config-file (files/parse-config-from-cli-arguments [])]
    (is (and (= 8080 (-> config-file :server :port)) (= "/var/lib/plauna/" (:data-folder config-file))))))

(deftest env-variables-override-defaults
  "Environment variablles override defaults."
  (binding [plauna.files/system-env (fn [key] (get {"SERVER_PORT" "3000" "DATA_FOLDER" "/var/test"} key))]
    (let [config-file (files/parse-config-from-cli-arguments [])]
      (is (and (= 3000 (-> config-file :server :port)) (= "/var/test" (:data-folder config-file)))))))

(deftest cli-arguments-override-env-variables
  "Cli arguments override environment variablles."
  (binding [plauna.files/system-env (fn [key] (get {"SERVER_PORT" "3000" "DATA_FOLDER" "/var/test"} key))]
    (let [config-file (files/parse-config-from-cli-arguments ["--server-port" "4000" "--data-folder" "/var/test2"])]
      (is (and (= 4000 (-> config-file :server :port)) (= "/var/test2" (:data-folder config-file)))))))
