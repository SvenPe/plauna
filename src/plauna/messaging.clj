(ns plauna.messaging
  (:require [clojure.core.async :refer [chan pub sub <! go-loop close!] :as async]))

(set! *warn-on-reflection* true)

(def main-chan (ref (chan 1000)))

(def main-publisher (ref (pub @main-chan :type)))

(defn restart-main-chan [] (dosync (alter main-chan (fn [old-chan] (close! old-chan)))
                                   (alter main-chan (fn [_] (chan 1000)))
                                   (alter main-publisher (fn [_] (pub @main-chan :type)))))

(defn stop! []
  (close! @main-chan))
(def limiter-limit (ref 300))

(defn channel-limiter
  "Backpressure for a producing operation: the producer puts a token on :bucket before each event it
   sends, and a go-loop drains one token per published event of the target type(s), so the producer
   can never run more than the bucket size ahead of the consumers. target-types is one event type or
   a set of them: a producer whose input can be DROPPED downstream (an unparseable mbox message never
   becomes a :parsed-enrichable-email) must also listen to the event the consumer emits for a drop,
   otherwise the orphaned tokens fill the bucket and the producer blocks forever.
   Returns a handle {:bucket ... :target ... :types ...}. The producer MUST call close-limiter! when
   it finishes (try/finally): the subscription and its go-loop otherwise leak, and every future event
   of these types keeps being delivered to the abandoned subscriber."
  [target-types]
  (let [types (if (set? target-types) target-types #{target-types})
        bucket-channel (chan @limiter-limit)
        target-channel (chan)]
    (doseq [target-type types]
      (sub @main-publisher target-type target-channel))
    (go-loop []
      (when-some [_ (<! target-channel)]
        ;; Consume a token without blocking: pub/mult only deliver the next event once ALL
        ;; subscribers accepted, so a blocking take on an empty bucket could park the whole topic.
        ;; The producer puts its token before its event, so the token is normally already here.
        (async/poll! bucket-channel)
        (recur)))
    {:bucket bucket-channel :target target-channel :types types}))

(defn close-limiter!
  "Unsubscribe and close a limiter created by channel-limiter, ending its go-loop. Events published
   after this simply skip the limiter; any in-flight ones no longer need tokens."
  [{:keys [bucket target types]}]
  (doseq [target-type types]
    (async/unsub @main-publisher target-type target))
  (close! target)
  (close! bucket))

(comment
  (restart-main-chan))
