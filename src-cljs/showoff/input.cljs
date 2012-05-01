(ns showoff.input
  (:require [goog.events :as gevents]
            [goog.events.KeyHandler :as geventskey]))

(def *command-state-map* #{})

(defn- keydown [e kbd-remapper]
  (set! *command-state-map*
        (conj *command-state-map* (kbd-remapper (.-keyCode e)))))

(defn- keyup [e kbd-remapper]
  (set! *command-state-map*
        (disj *command-state-map* (kbd-remapper (.-keyCode e)))))

(defn prepare [kbd-remapper]
  (let [doc js/document]
    (gevents/listen doc (.-KEYDOWN gevents/EventType)
                    (fn [e] (keydown e kbd-remapper)))
    
    (gevents/listen doc (.-KEYUP gevents/EventType)
                    (fn [e] (keyup e kbd-remapper)))))

(defn state? [key]
  (*command-state-map* key))

(defn state []
  *command-state-map*)

(defn standard-remapper [key]
  (condp = key
    (.-LEFT gevents/KeyCodes) :left
    (.-RIGHT gevents/KeyCodes) :right
    (.-UP gevents/KeyCodes) :up
    (.-DOWN gevents/KeyCodes) :down
    32 :space

    ;; else
    key))