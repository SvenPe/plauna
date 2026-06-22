(ns plauna.diagnostics
  "Runtime diagnostics for investigating freezes.

   A freeze with no exception is almost always a thread that is blocked (a lock deadlock, or a thread
   waiting on I/O that never returns). The tools here make that visible in the logs:
   - a background watchdog that periodically checks for JVM deadlocks and logs the full stack traces
     of the involved threads (it runs on its own daemon thread, so it keeps working even when the
     rest of the app is stuck),
   - an on-demand full thread dump (used by the /admin/threads endpoint),
   - a default uncaught-exception handler so a thread that dies silently still leaves a log entry."
  (:require [clojure.string :as str]
            [taoensso.telemere :as t])
  (:import (java.lang.management ManagementFactory ThreadInfo ThreadMXBean)
           (java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit)))

(set! *warn-on-reflection* true)

(defn- thread-mx-bean ^ThreadMXBean [] (ManagementFactory/getThreadMXBean))

(defn- format-thread-info [^ThreadInfo ti]
  (let [sb (StringBuilder.)]
    (.append sb (format "\"%s\" id=%d %s" (.getThreadName ti) (.getThreadId ti) (str (.getThreadState ti))))
    (when-let [lock (.getLockName ti)] (.append sb (str " waiting on " lock)))
    (when-let [owner (.getLockOwnerName ti)]
      (.append sb (format " (lock held by \"%s\" id=%d)" owner (.getLockOwnerId ti))))
    (.append sb "\n")
    (doseq [^StackTraceElement el (.getStackTrace ti)]
      (.append sb (str "        at " el "\n")))
    (.toString sb)))

(defn thread-dump-string
  "A formatted dump of all live threads with full stack traces and lock ownership."
  []
  (let [infos (.dumpAllThreads (thread-mx-bean) true true)]
    (str "===== THREAD DUMP (" (alength infos) " threads) =====\n"
         (str/join "\n" (map format-thread-info infos)))))

(defn deadlock-report
  "If the JVM detects a deadlock (cyclic lock dependency), return a formatted report of the involved
   threads; otherwise nil."
  []
  (let [bean (thread-mx-bean)
        ids (.findDeadlockedThreads bean)]
    (when (some? ids)
      (let [infos (.getThreadInfo bean ids true true)]
        (str "===== DEADLOCK DETECTED (" (alength ids) " threads involved) =====\n"
             (str/join "\n" (map format-thread-info infos)))))))

(defn log-thread-dump!
  "Log a full thread dump at :warn. reason is a short string describing why it was taken."
  ([] (log-thread-dump! "manual"))
  ([reason] (t/log! :warn (str "Thread dump (" reason "):\n" (thread-dump-string)))))

(defonce ^:private watchdog (atom nil))

(defn- watchdog-thread-factory []
  (reify ThreadFactory
    (newThread [_ r] (doto (Thread. ^Runnable r "plauna-watchdog") (.setDaemon true)))))

(defn start-watchdog!
  "Start a daemon task that checks for JVM deadlocks every interval-seconds and logs the stack traces
   of any deadlocked threads at :error. No-op if already running."
  [interval-seconds]
  (when (nil? @watchdog)
    (let [^ScheduledExecutorService ex (Executors/newSingleThreadScheduledExecutor (watchdog-thread-factory))]
      (.scheduleWithFixedDelay ex
                               ^Runnable (fn []
                                           (try
                                             (when-let [report (deadlock-report)]
                                               (t/log! :error (str "Watchdog detected a deadlock. The application is likely frozen.\n" report)))
                                             (catch Throwable e
                                               (t/log! {:level :error :error e} "Watchdog check failed"))))
                               (long interval-seconds) (long interval-seconds) TimeUnit/SECONDS)
      (reset! watchdog ex)
      (t/log! :info ["Started freeze watchdog: checking for deadlocks every" interval-seconds "seconds."]))))

(defn stop-watchdog! []
  (when-let [^ScheduledExecutorService ex @watchdog]
    (.shutdownNow ex)
    (reset! watchdog nil)))

(defn install-uncaught-exception-handler!
  "Log any exception that escapes a thread, so silently dying threads still leave evidence."
  []
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (t/log! {:level :error :error ex} (str "Uncaught exception in thread \"" (.getName ^Thread thread) "\""))))))
