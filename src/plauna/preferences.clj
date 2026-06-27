(ns plauna.preferences
  (:require
   [clojure.core.cache.wrapped :as w]
   [plauna.settings :as settings]))

(def cache (w/ttl-cache-factory {} :ttl 6000))

(def fetch-fn (atom settings/fetch-setting))

(def converters {clojure.lang.Keyword (fn [^String s] (keyword (cond-> s (.startsWith s ":") (.substring 1))))
                 java.lang.Double (fn [v] (if (instance? Number v) (double v) (Double/parseDouble (str v))))
                 java.lang.Long   (fn [v] (if (instance? Number v) (long v)   (Long/parseLong (str v))))})

(defmacro preference-with-default [property pred default]
  `(let [value# (~pred (@fetch-fn ~property) ~default)
         default-type# (class ~default)
         type# (class value#)]
     (if (= default-type# type#)
       value#
       ((get converters default-type#) value#))))

(defn update-preference [key value]
  (settings/update-setting! key value)
  (w/evict cache key))

(defn log-level [] (w/lookup-or-miss cache
                                     :log-level
                                     (fn [key] (preference-with-default key or :info))))

(defn language-detection-threshold [] (w/lookup-or-miss cache
                                                        :language-detection-threshold
                                                        (fn [key] (preference-with-default key or 0.80))))

(defn categorization-threshold [] (w/lookup-or-miss cache
                                                    :categorization-threshold
                                                    (fn [key] (preference-with-default key or 0.65))))

(defn client-health-check-interval [] (w/lookup-or-miss cache
                                                        :client-health-check-interval
                                                        (fn [key] (preference-with-default key or 60))))
