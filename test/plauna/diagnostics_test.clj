(ns plauna.diagnostics-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [plauna.diagnostics :as d]))

(deftest thread-dump-includes-running-threads
  (let [dump (d/thread-dump-string)]
    (is (str/includes? dump "THREAD DUMP"))
    (is (str/includes? dump "        at ") "Stack frames are included"))
  "thread-dump-string produces a readable dump with stack frames")

(deftest no-deadlock-under-normal-conditions
  (is (nil? (d/deadlock-report)) "No deadlock is reported when the JVM is healthy")
  "deadlock-report returns nil when there is no deadlock")

(deftest watchdog-starts-and-stops
  (try
    (d/start-watchdog! 60)
    (is (some? @@#'d/watchdog) "Watchdog is running after start")
    (finally
      (d/stop-watchdog!)
      (is (nil? @@#'d/watchdog) "Watchdog is cleared after stop")))
  "The watchdog can be started and stopped")
