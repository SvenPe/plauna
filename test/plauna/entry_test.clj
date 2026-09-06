(ns plauna.entry-test
  (:require [clojure.test :refer :all]
            [plauna.entry :as entry]))

(deftest connection-retries-back-off-exponentially-and-are-capped
  (is (= 120 (entry/next-retry-delay-seconds 1)))
  (is (= 240 (entry/next-retry-delay-seconds 2)))
  (is (= 480 (entry/next-retry-delay-seconds 3)))
  (is (= 1920 (entry/next-retry-delay-seconds 5)))
  (is (= entry/connection-retry-max-backoff-seconds (entry/next-retry-delay-seconds 6)))
  (is (= entry/connection-retry-max-backoff-seconds (entry/next-retry-delay-seconds 40))
      "A wrong password does not hammer the provider every two minutes forever")
  "Failed connection attempts are retried with a doubling pause of at most an hour")
