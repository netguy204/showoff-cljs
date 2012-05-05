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

(defn curry [fun & args]
  (fn [last]
    (let [args (into [] args)
          full-args (conj args last)]
      (apply fun full-args))))

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

;; end jayq

(defn no-cache [resource]
  (str resource "?" (Math/random)))

(defn with-loaded-assets [keys-and-loaders callback]
  "load all requested assets in parallel and then activate callback
with a map of those assets when they're ready"
  (let [results (atom {})]
    (doseq [[key loader] keys-and-loaders]
      (loader
       (fn [asset]
         (swap! results conj {key asset})
         (when (= (count @results) (count keys-and-loaders))
           (callback @results)))))))

