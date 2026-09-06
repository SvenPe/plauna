(ns plauna.util.sql
  "Small dialect-neutral SQL helpers shared by the query builders in plauna.application and the
   database layer."
  (:require [clojure.string :as str]))

(defn escape-like
  "Escape LIKE wildcards (% and _) and the escape character itself so user input matches literally.
   Must be paired with an explicit ESCAPE '\\' in the LIKE expression: SQLite has no default escape
   character, so without it '_' matches any character and '%' matches everything."
  [text]
  (str/replace (str text) #"([\\%_])" "\\\\$1"))

(defn like-contains
  "A honeysql LIKE condition matching rows whose column contains text literally."
  [column text]
  [:like column [:escape (str "%" (escape-like text) "%") "\\"]])
