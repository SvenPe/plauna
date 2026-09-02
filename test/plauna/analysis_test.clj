(ns plauna.analysis-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [plauna.analysis :as analysis]))

;; Normalization tests

(deftest normalization-1
  (let [res (analysis/normalize (slurp (io/resource "test/normalization/original-text-1.txt")))]
    (is (= (s/trim (slurp (io/resource "test/normalization/normalized-text-1.txt"))) res))))

(deftest normalization-2
  (let [res (analysis/normalize (slurp (io/resource "test/normalization/greek-lorem-ipsum.txt")))]
    (is (= (s/trim (slurp (io/resource "test/normalization/normalized-greek-lorem-ipsum.txt"))) res))))

(deftest training-and-prediction-share-namespaced-features
  (let [email {:header {:subject "Invoice Ready"}
               :participants [{:type :sender :address "Billing@Example.COM"}
                              {:type :receiver :address "me@example.net"}]
               :body [{:mime-type "text/plain"
                       :content (s/join " " (repeat 510 "Payment"))}]
               :metadata {:category-id 7}}
        tokens (analysis/classification-tokens email)
        training-line (analysis/format-training-data [email])]
    (is (= ["sender-address:billing@example.com"
            "sender-domain:example.com"
            "subject:invoice"
            "subject:ready"]
           (subvec tokens 0 4)))
    (is (= analysis/max-body-features
           (count (filter #(s/starts-with? % "body:") tokens)))
        "Long bodies are bounded before either training or prediction")
    (is (= (str "7 " (s/join " " tokens) "\n") training-line)
        "The serialized training example contains exactly the prediction features")))

(deftest legacy-models-keep-their-original-body-only-features
  (let [email {:header {:subject "New Subject"}
               :participants [{:type :sender :address "sender@example.com"}]
               :body [{:mime-type "text/plain" :content "Legacy Body Tokens"}]}]
    (is (= ["Legacy" "Body" "Tokens"]
           (analysis/legacy-classification-tokens email)))
    (is (not-any? #(or (s/starts-with? % "sender-")
                       (s/starts-with? % "subject:")
                       (s/starts-with? % "body:"))
                  (analysis/legacy-classification-tokens email)))
    (is (= (analysis/legacy-classification-tokens email)
           (analysis/classification-tokens-for-model
            email "eng" (java.io.File. "train-eng.bin"))))
    (is (= (analysis/classification-tokens email)
           (analysis/classification-tokens-for-model
            email "eng" (java.io.File. "train-eng-maxent.bin"))))))

(deftest categorization-model-names-map-to-opennlp-trainers
  (is (= "NAIVEBAYES" (analysis/categorization-algorithm "naive-bayes")))
  (is (= "MAXENT" (analysis/categorization-algorithm "maxent"))))

(deftest naive-bayes-and-maxent-can-train-the-same-feature-data
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")
        model-file (java.io.File/createTempFile "plauna-model-" ".bin")
        samples (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                     "1 sender-domain:shop.example subject:receipt body:purchase\n"
                     "1 sender-domain:billing.example subject:invoice body:amount\n"
                     "2 sender-domain:friends.example subject:dinner body:tomorrow\n"
                     "2 sender-domain:friends.example subject:weekend body:meeting\n"
                     "2 sender-domain:club.example subject:invitation body:party\n")]
    (try
      (spit training-file samples)
      (doseq [model ["naive-bayes" "maxent"]]
        (let [trained (first (analysis/train-data [{:language "eng" :file training-file}] model))]
          (with-open [os (io/output-stream model-file)]
            (analysis/serialize-and-write-model! (:model trained) os))
          (is (= "1" (:name (analysis/categorize-tokens
                              ["sender-domain:shop.example" "subject:invoice" "body:payment"]
                              model-file)))
              (str model " produces a readable classifier"))))
      (finally
        (io/delete-file training-file true)
        (io/delete-file model-file true)))))

(deftest maxent-training-reports-iteration-progress
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")
        events (atom [])]
    (try
      (spit training-file (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                               "2 sender-domain:friends.example subject:dinner body:tomorrow\n"))
      (let [trained (analysis/train-data [{:language "eng" :file training-file}] "maxent" #(swap! events conj %))]
        (is (= 1 (count trained)))
        (is (some? (:model (first trained))))
        (is (= {:language "eng" :language-index 1 :languages 1 :iteration 0 :iterations analysis/training-iterations}
               (first @events))
            "Training announces the language before the first iteration")
        (is (some #(and (pos? (:iteration %)) (= "eng" (:language %))) @events)
            "MaxEnt reports finished iterations")
        (is (:done? (last @events)) "The final event marks the end of training"))
      (finally
        (io/delete-file training-file true)))))

(deftest training-file-outcomes-counts-samples-and-labels
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")]
    (try
      (spit training-file "1 subject:a body:b\n1 subject:c\n\n2 subject:d\n")
      (is (= {:samples 3 :labels #{"1" "2"}} (analysis/training-file-outcomes training-file)))
      (finally (io/delete-file training-file true)))))

(deftest a-failing-language-does-not-abort-the-other-models
  (let [good (java.io.File/createTempFile "plauna-training-good-" ".train")
        single (java.io.File/createTempFile "plauna-training-single-" ".train")]
    (try
      (spit good (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                      "2 sender-domain:friends.example subject:dinner body:tomorrow\n"))
      (spit single "1 sender-domain:shop.example subject:invoice body:payment\n")
      (let [results (analysis/train-data [{:language "eng" :file good} {:language "deu" :file single}] "naive-bayes")]
        (is (= ["eng" "deu"] (mapv :language results)))
        (is (some? (:model (first results))) "The trainable language still yields a model")
        (is (nil? (:model (second results))))
        (is (instance? Exception (:error (second results))) "The single-outcome language reports its error instead of throwing"))
      (finally
        (io/delete-file good true)
        (io/delete-file single true)))))
