(ns plauna.util.page
  (:require [clojure.math :refer [ceil]]))

(defrecord PageRequest [page size])

(defn page-request [page size]
  (->PageRequest (or page 1) (or size 10)))

(defn calculate-pages-total [total size] (max 1 (int (ceil (/ (double total) size)))))

(defn page-request->limit-offset [page-request]
  ;; Clamp page to >= 1 so the offset can never go negative. A negative OFFSET is silently treated as 0
  ;; by SQLite but is a syntax error on MariaDB (e.g. callers that pass {:page 0}).
  (let [size (:size page-request)
        page (max 1 (:page page-request))]
    {:limit size :offset (* size (dec page))}))
