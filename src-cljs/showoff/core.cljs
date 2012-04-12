(ns showoff.core
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.string.format :as format)))

(defn by-id [id]
  (dom/getElement id))

(defn content []
  (by-id "content"))

(def ^:dynamic *display* nil)

(defn prepare-display []
  (set! *display* (dom/createDom "canvas"))
  (set! (.-width *display*) 640)
  (set! (.-height *display*) 480)
  (dom/appendChild (content) *display*))

(defn context []
  (.getContext *display* "2d"))

(defn color [r g b & [a]]
  (if (= a nil)
    (string/format "rgb(%f,%f,%f)" r g b)
    (string/format "rgba(%f,%f,%f,%f)" r g b a)))

(defn filled-rect [ctx [x y] [w h] color]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx x y w h))

(defn ^:export main []
  (dom/setTextContent (content) "")
  (prepare-display)
  (doto (context)
    (filled-rect [10 10] [55 50] (color 200 0 0))
    (filled-rect [30 30] [55 50] (color 0 0 200 0.5))))


