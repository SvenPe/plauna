(ns plauna.analysis
  (:require [clojure.string :as st]
            [plauna.database :as db]
            [plauna.core.events :as events]
            [plauna.core.email :as core-email]
            [plauna.preferences :as p]
            [plauna.util.text-transform :as tt]
            [clojure.core.async :as async]
            [taoensso.telemere :as t]
            [cld.core :as lang]
            [plauna.files :as files]
            [plauna.interfaces :as int])
  (:import
   (opennlp.tools.util.normalizer AggregateCharSequenceNormalizer NumberCharSequenceNormalizer ShrinkCharSequenceNormalizer CharSequenceNormalizer)
   (opennlp.tools.util MarkableFileInputStreamFactory PlainTextByLineStream TrainingParameters)
   (opennlp.tools.doccat DocumentSampleStream DocumentCategorizerME DoccatFactory DoccatModel)
   (opennlp.tools.ml.maxent GISTrainer)
   (opennlp.tools.ml.naivebayes NaiveBayesTrainer)
   (java.util Locale)
   (java.util.regex Pattern)
   (java.io File OutputStream)))

(set! *warn-on-reflection* true)

(def BracketsNormalizer (reify CharSequenceNormalizer
                          (normalize [_ text] ((comp st/trim #(st/replace % #"\( \)" "")) text))))

(def MailtoNormalizer (reify CharSequenceNormalizer
                        (normalize [_ text] ((comp st/trim #(st/replace % #"(mailto:)?(?<![-+_.0-9A-Za-z])[-+_.0-9A-Za-z]+@[-0-9A-Za-z]+[-.0-9A-Za-z]+" "")) text))))

(def BetterURLNormalizer (reify CharSequenceNormalizer
                           (normalize [_ text] ((comp st/trim #(st/replace % #"https?://[-_.?&~%;+=/#0-9A-Za-z]+" "")) text))))

(def NonCharNormalizer (reify CharSequenceNormalizer
                         (normalize [_ text] (#(st/replace % (Pattern/compile "[^\\s\\w]" (bit-or Pattern/MULTILINE Pattern/UNICODE_CHARACTER_CLASS)) " ") text))))

(def ExtraWhiteSpaceNormalizer (reify CharSequenceNormalizer
                                 (normalize [_ text] (#(st/replace % (Pattern/compile "\\s{2,}" Pattern/MULTILINE) " ") text))))

(def NonPrintableCharNormalizer (reify CharSequenceNormalizer
                                  (normalize [_ text] (#(st/replace % (Pattern/compile "\\p{C}") " ") text))))

(def ^CharSequenceNormalizer normalizer (new AggregateCharSequenceNormalizer
                                             (into-array CharSequenceNormalizer
                                                         [BetterURLNormalizer
                                                          MailtoNormalizer
                                                          (NumberCharSequenceNormalizer/getInstance)
                                                          BracketsNormalizer
                                                          NonPrintableCharNormalizer
                                                          NonCharNormalizer
                                                          (ShrinkCharSequenceNormalizer/getInstance)
                                                          ExtraWhiteSpaceNormalizer])))

(defn normalize [^String text] (.normalize normalizer text))

(def supported-categorization-models
  [{:id "naive-bayes" :name "Naive Bayes"}
   {:id "maxent" :name "Maximum Entropy (MaxEnt)"}])

(defn categorization-algorithm
  (^String [] (categorization-algorithm (p/categorization-model)))
  (^String [model]
   (case (p/canonical-categorization-model model)
     "maxent" GISTrainer/MAXENT_VALUE
     NaiveBayesTrainer/NAIVE_BAYES_VALUE)))

(lang/default-init!)

(defn lang-code-set3 [language]
  ;; forLanguageTag handles hyphenated langdetect codes like "zh-cn"/"zh-tw" (new Locale would throw
  ;; MissingResourceException on .getISO3Language). Fall back to the raw code if no ISO3 form exists.
  (try
    (let [iso3 (.getISO3Language (Locale/forLanguageTag language))]
      (if (st/blank? iso3) language iso3))
    (catch Exception _ language)))

(defn detect-language [^String text]
  (when (some? text)
    (try
      (if (> (count text) 3)
        ;; lang/detect returns [best-language probabilities-map]. Use the explicitly detected best
        ;; language and look up its probability, rather than relying on the (unordered) map's first entry.
        (let [[best-lang probabilities] (lang/detect text)
              confidence (Double/parseDouble (or (get probabilities best-lang) "0.0"))
              lang-code (lang-code-set3 best-lang)]
          {:code (if (< confidence (p/language-detection-threshold)) "n/a" lang-code)
           :confidence confidence})
        {:code "n/a" :confidence 0.0})
      (catch com.cybozu.labs.langdetect.NoFeatureInTextException e
        (t/log! {:level :error :error e} "There was an error when detecting the language")
        (t/log! :debug ["The following text threw an exception:" text])
        {:code "n/a" :confidence 0.0}))))

(defn training-data-stream [file]
  (-> (MarkableFileInputStreamFactory. file)
      (PlainTextByLineStream. "UTF-8")
      (DocumentSampleStream.)))

(defn training-body-part [email] (core-email/body-part-for-mime-type "text/html" email))

(def max-body-features
  "Bound long messages so repeated boilerplate cannot overwhelm sender and subject evidence."
  500)

(defn- normalized-words [text]
  (if (st/blank? text)
    []
    (map st/lower-case
         (remove st/blank? (st/split (normalize text) #"\s+")))))

(defn- sender-addresses [email]
  (->> (:participants email)
       (filter #(contains? #{:sender "sender"} (:type %)))
       (keep :address)
       (map st/lower-case)
       (map st/trim)
       (remove st/blank?)
       distinct))

(defn- sender-domain [address]
  (let [separator (.lastIndexOf ^String address "@")]
    (when (< -1 separator (dec (count address)))
      (subs address (inc separator)))))

(defn classification-feature-groups
  "Build the exact same namespaced features for training and prediction. Keeping sender address,
   domain, subject and body in separate namespaces prevents an identical word from being treated as
   the same signal everywhere."
  [email]
  (let [addresses (sender-addresses email)
        subject (get-in email [:header :subject])
        body-part (training-body-part email)
        body-text (when body-part
                    (tt/clean-text-content (:content body-part)
                                           (core-email/text-content-type body-part)))]
    {:sender (concat (map #(str "sender-address:" %) addresses)
                     (keep #(when-let [domain (sender-domain %)]
                              (str "sender-domain:" domain))
                           addresses))
     :subject (map #(str "subject:" %) (normalized-words subject))
     :body (map #(str "body:" %) (take max-body-features (normalized-words body-text)))}))

(defn classification-tokens [email]
  (let [{:keys [sender subject body]} (classification-feature-groups email)]
    (vec (concat sender subject body))))

(defn legacy-classification-tokens
  "Reproduce the pre-migration prediction input for a legacy train-<lang>.bin model. Those models do
   not know the new feature namespaces and would otherwise see every token as unknown."
  [email]
  (let [body-part (training-body-part email)]
    (if body-part
      (let [content (tt/clean-text-content (:content body-part)
                                           (core-email/text-content-type body-part))]
        (if (st/blank? content)
          []
          (vec (remove st/blank? (st/split (normalize content) #" ")))))
      [])))

(defn classification-tokens-for-model [email language-code ^File model-file]
  (if (= (.getName model-file) (str "train-" language-code ".bin"))
    (legacy-classification-tokens email)
    (classification-tokens email)))

(defn format-training-data [data]
  ;; The model label is the category ID, not its name: DocumentSampleStream treats the first
  ;; whitespace-delimited token as the label, so a name like "Work Projects" would be trained
  ;; as "Work" and never resolve back to a category. IDs are single tokens by construction.
  (transduce
   (comp (map (fn [email] [(get-in email [:metadata :category-id])
                           (classification-tokens email)]))
         (filter (fn [[category tokens]] (and (some? category) (seq tokens))))
         (map (fn [[category tokens]]
                (str category " " (st/join " " tokens) "\n"))))
   str
   ""
   data))

(defn training-parameters
  ([] (training-parameters (p/categorization-model)))
  ([model]
   (doto (new TrainingParameters)
     (.put TrainingParameters/ITERATIONS_PARAM 1000)
     (.put TrainingParameters/CUTOFF_PARAM 0)
     (.put TrainingParameters/ALGORITHM_PARAM (categorization-algorithm model)))))

(comment NaiveBayesTrainer/NAIVE_BAYES_VALUE
         GISTrainer/MAXENT_VALUE
         "")

(defn serialize-and-write-model! [^DoccatModel model ^OutputStream os]
  (when (some? model) (.serialize model os)))

(defn train-data
  ([training-files] (train-data training-files (p/categorization-model)))
  ([training-files model]
   (mapv (fn [tf]
           {:model (DocumentCategorizerME/train (:language tf)
                                                (training-data-stream (:file tf))
                                                (training-parameters model)
                                                (DoccatFactory.))
            :language (:language tf)})
         training-files)))

(defn categorize-tokens [tokens ^File model-file]
  (if (and (.exists model-file) (seq tokens))
    (let [doccat (DocumentCategorizerME. (DoccatModel. model-file))
          cat-results (.categorize doccat (into-array String tokens))
          best-category (.getBestCategory doccat cat-results)
          best-probability (get cat-results (.getIndex doccat best-category))]
      (if (> best-probability (p/categorization-threshold))
        {:name best-category :confidence best-probability}
        {:name nil :confidence 0}))
    {:name nil :confidence 0}))

(defn categorize [text ^File model-file]
  (categorize-tokens (when-not (st/blank? text) (st/split text #"\s+")) model-file))

(defn normalize-body-part [body-part]
  (when (some? body-part)
    (normalize (tt/clean-text-content (:content body-part) (core-email/text-content-type body-part)))))

(defn label->category
  "Resolve a model label to its category row. Labels are category ids (see format-training-data);
   fall back to a name lookup so models trained before ids were used as labels keep working until
   the next re-training. Returns nil when the label matches no existing category."
  [label]
  (when (some? label)
    (or (when-let [id (parse-long (str label))] (db/category-by-id id))
        (db/category-by-name label))))

(defn category-for-email [email language-code]
  (when (and (some? email) (some? language-code))
    (let [allowed-languages (mapv :language (db/get-activated-language-preferences))]
      (when (some #(= language-code %) allowed-languages)
        (let [model-file (files/model-file language-code (p/categorization-model))
              tokens (classification-tokens-for-model email language-code model-file)]
          (categorize-tokens tokens model-file))))))

(defn detect-language-and-categorize-event [event]
  (let [email (:payload event)
        body-part-to-train-on (core-email/body-part-for-mime-type "text/html" email)
        training-content (normalize-body-part body-part-to-train-on)
        language-result (detect-language training-content)
        category-result (category-for-email email (:code language-result))
        category (label->category (:name category-result))]
    (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category (:name category) :category-confidence (:confidence category-result) :category-id (:id category)})))

(defn detect-language-and-categorize-email [email]
  (let [body-part-to-train-on (core-email/body-part-for-mime-type "text/html" email)
        training-content (normalize-body-part body-part-to-train-on)
        language-result (detect-language training-content)
        category-result (category-for-email email (:code language-result))
        category (label->category (:name category-result))]
    (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category (:name category) :category-confidence (:confidence category-result) :category-id (:id category)})))

(defn detect-language-event [event]
  (let [email (:payload event)
        body-part-to-train-on (core-email/body-part-for-mime-type "text/html" email)
        training-content (normalize-body-part body-part-to-train-on)
        language-result (try (detect-language training-content) (catch Exception e (t/log! {:level :error :error e} [(.getMessage e) "\nText causing the exception:" training-content])))]
    (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category (-> email :metadata :category) :category-confidence (-> email :metadata :category-confidence) :category-id (-> email :metadata :category-id)})))

(defn language-result [email]
  (let [body-part-to-train-on (core-email/body-part-for-mime-type "text/html" email)
        training-content (normalize-body-part body-part-to-train-on)]
    (try (detect-language training-content) (catch Exception e (t/log! {:level :error :error e} [(.getMessage e) "\nText causing the exception:" training-content])))))

(defmulti handle-enrichment :type)

(defmethod handle-enrichment :parsed-enrichable-email [event]
  (events/create-event :enriched-email (detect-language-and-categorize-event event) nil event))

(defmethod handle-enrichment :language-detection-request [event]
  (events/create-event :enriched-email (detect-language-event event) nil event))

(defn enrichment-event-loop
  "Enriches the e-mails. Listens to two events:

  :parsed-enrichable-email    - Detects both the language and the category
  :language-detection-request - Only detects the language"
  [publisher events-channel]
  (let [parsed-enrichable-email-chan (async/chan)
        language-detection-request-chan (async/chan)
        local-chan (async/merge [parsed-enrichable-email-chan language-detection-request-chan])]
    (async/sub publisher :parsed-enrichable-email parsed-enrichable-email-chan)
    (async/sub publisher :language-detection-request language-detection-request-chan)
    (async/pipeline 4
                    events-channel
                    (map handle-enrichment)
                    local-chan
                    true
                    (fn [^Throwable th] (t/log! {:level :error :error th} (.getMessage th))))))

(defrecord BasicAnalyzer []
  int/Analyzer
  (enrich-email [_ email] (detect-language-and-categorize-email email))
  (detect-language [_ email] (language-result email)))
