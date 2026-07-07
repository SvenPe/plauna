(ns plauna.messaging
  (:require [clojure.core.async :refer [chan pub sub <! go-loop close!] :as async]))

(set! *warn-on-reflection* true)

(def main-chan (ref (chan 1000)))

(def main-publisher (ref (pub @main-chan :type)))

(defn restart-main-chan [] (dosync (alter main-chan (fn [old-chan] (close! old-chan)))
                                   (alter main-chan (fn [_] (chan 1000)))
                                   (alter main-publisher (fn [_] (pub @main-chan :type)))))
(def limiter-limit (ref 300))

(defn channel-limiter [target-type]
  (let [bucket-channel (chan @limiter-limit)
        target-channel (chan)]
    (sub @main-publisher target-type target-channel)
    (go-loop []
      (when-some [_ (<! target-channel)]
        ;; Consume a token without blocking. A limiter is never unsubscribed, so a blocking take on an
        ;; abandoned limiter's empty bucket would park this loop while its target-channel still receives
        ;; every event — and since pub/mult only deliver the next event once ALL subscribers accepted,
        ;; one stale limiter would freeze the whole topic (e.g. on the second mbox upload). The producer
        ;; puts its token before its event, so for a live limiter the token is always already here.
        (async/poll! bucket-channel)
        (recur)))
    bucket-channel))

(comment
  (restart-main-chan))
