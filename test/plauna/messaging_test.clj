(ns plauna.messaging-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async]
            [plauna.messaging :as messaging]))

(deftest limiter-drains-tokens-for-every-listed-event-type
  (let [original @messaging/limiter-limit]
    (dosync (ref-set messaging/limiter-limit 1))
    (let [limiter (messaging/channel-limiter #{:parsed-enrichable-email :discarded-email})
          bucket (:bucket limiter)
          offered-after (fn [] (loop [tries 0]
                                 (cond (async/offer! bucket :token) true
                                       (< tries 100) (do (Thread/sleep 20) (recur (inc tries)))
                                       :else false)))]
      (try
        (is (true? (async/offer! bucket :token)) "The bucket takes one token")
        (is (not (async/offer! bucket :token)) "...and is then full")
        (async/>!! @messaging/main-chan {:type :discarded-email :payload {:reason :test}})
        (is (true? (offered-after)) "A discarded-email event frees a token")
        (async/>!! @messaging/main-chan {:type :parsed-enrichable-email :payload {}})
        (is (true? (offered-after)) "A parsed event frees a token too")
        (finally
          (messaging/close-limiter! limiter)
          (dosync (ref-set messaging/limiter-limit original))))))
  "An mbox import whose fragments are dropped downstream no longer blocks once the bucket is full of orphaned tokens")

(deftest a-single-type-limiter-still-works
  (let [limiter (messaging/channel-limiter :enriched-email)]
    (try
      (is (= #{:enriched-email} (:types limiter)))
      (finally (messaging/close-limiter! limiter)))))
