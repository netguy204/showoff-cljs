(ns showoff.timer
  (:use [showoff.showoff :only [Tickable tick]]))

(def *live-timers* nil)
(def *next-callback* nil)

(defprotocol Timer
  (fire [t])
  (time-point [t]))

(defn- remove-timer [timer]
  (set! *live-timers* (filter #(identical? % timer) *live-timers*))
  (if *live-timers*
    (set! *next-callback* (time-point (first *live-timers*)))
    (set! *next-callback* nil)))

(defn- add-timer [timer]
  (set! *live-timers* (sort-by time-point (conj *live-timers* timer)))
  (if *next-callback*
    (set! *next-callback* (min *next-callback* (time-point timer)))
    (set! *next-callback* (time-point timer))))

(defrecord OneShotTimer [target-time callback]
  Timer
  (fire [timer]
    (callback)
    (remove-timer timer))

  (time-point [t] target-time))

(defrecord PeriodicTimer [next-target-time period callback]
  Timer
  (fire [timer]
    (callback)
    (reset! next-target-time (+ (goog/now) period)))

  (time-point [timer] @next-target-time))

(defn callback-in [timeout-ms callback]
  (add-timer (OneShotTimer. (+ (goog/now) timeout-ms) callback)))

(defn periodic [period-ms callback]
  (add-timer (PeriodicTimer. (atom (+ (goog/now) period-ms)) period-ms callback)))

(defn service []
  (let [now (goog/now)]
    (when (>= now *next-callback*)
      (loop [timers *live-timers*]
        (if-let [timer (first timers)]
         (when (>= now (time-point timer))
           (fire timer)
           (recur (next timers))))))))