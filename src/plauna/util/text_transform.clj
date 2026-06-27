(ns plauna.util.text-transform
  (:require
   [clojure.string :as str]
   [clojure.java.io :refer [input-stream]])
  (:import
   (java.nio.charset StandardCharsets)
   (org.jsoup Jsoup)
   (javax.swing.text.rtf RTFEditorKit)))

(defn html->text [^String html] (if (str/blank? html) "" (.text (Jsoup/parse html "UTF-8"))))

(defn rtf->string [^String rtf]
  (if (str/blank? rtf)
    ""
    (let [rtf-parser (new RTFEditorKit)
          document (.createDefaultDocument rtf-parser)]
      (.read rtf-parser (input-stream (.getBytes rtf StandardCharsets/UTF_8)) document 0)
      (.getText document 0 (.getLength document)))))

(defn clean-text-content [content content-type]
  (if (str/blank? content)
    ""
    (cond (= :html content-type) (html->text content)
          (= :rtf content-type) (rtf->string content)
          ;; Plain text: return as-is. Running it through Jsoup would strip anything resembling a tag
          ;; (e.g. "a < b", URLs in angle brackets).
          :else content)))
