(ns showoff.utils
  (:require [goog.string :as string]
            [goog.string.format :as format]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]))

(def format string/format)

(defn by-id [id]
  (dom/getElement id))

(defn append-child [parent child]
  (dom/appendChild parent child))

(defn set-class [div class]
  (classes/set div class))

(defn remove-class [div class]
  (classes/remove div class))

(defn find-pos [node]
  (when (.-offsetParent node)
    (loop [obj node
           left (.-offsetLeft obj)
           top (.-offsetTop obj)]
      (let [next (.-offsetParent obj)]
        (if next
          (recur next
                 (+ left (.-offsetLeft next))
                 (+ top (.-offsetTop next)))
          [left top])))))

(defn div-with-class [class content]
  (let [div (dom/createDom "div")]
    (classes/set div class)
    (append-child div content)
    div))

;; borrowed from ibdknox/jayq
(defn map->js [m]
  (let [out (js-obj)]
    (doseq [[k v] m]
      (aset out (name k) v))
    out))

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.-strobj (reduce (fn [m [k v]]
                                 (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))

(defn no-cache [resource]
  (str resource "?" (Math/random)))

;; end jayq
