(ns showoff.events
  (:require [showoff.utils :as util]
            [goog.events :as gevents]
            [goog.events.EventTarget :as event-target]))

(defn make-bus []
  (gevents/EventTarget.))

(defn make [type data]
  (let [obj (js-obj)]
    (aset obj "type" type)
    (aset obj "data" data)
    obj))

(defn kind [ev]
  (aget ev "type"))

(defn data [ev]
  (.-data ev))

(def *default-bus* (make-bus))

(defn dispatch-event
  ([ev] (dispatch-event ev *default-bus*))

  ([ev bus] (.dispatchEvent bus ev)))

(defn listen
  ([type handler] (listen type handler *default-bus*))
  ([type handler bus] (gevents/listen bus type #(handler (data %)))))

(defn listen-once
  ([type handler] (listen-once type handler *default-bus*))
  ([type handler bus] (gevents/listenOnce bus type #(handler (data %)))))
