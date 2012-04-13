(ns showoff.core
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.string.format :as format)
            (goog.events :as gevents)
            (goog.events.KeyHandler :as geventskey)
            (clojure.browser.event :as event)))

(defn by-id [id]
  (dom/getElement id))

(defn content []
  (by-id "content"))

(def ^:dynamic *display* nil)
(def ^:dynamic *viewport* nil) ;; expressed in tiles
(def ^:dynamic *world-dims* [640 480]) ;; expressed in pixels
(def ^:dynamic *tile-dims* [8 8])
(def *default-color* [255 0 255])

(defn prepare-display []
  (set! *display* (dom/createDom "canvas"))
  (set! *viewport* [0 0 32 24])
  (set! (.-width *display*) (nth *world-dims* 0))
  (set! (.-height *display*) (nth *world-dims* 1))
  (dom/appendChild (content) *display*))

(defn transform [[x y w h]]
  "convert from x y w h in tilespace to a position in visible screen
space taking into account the current viewport"
  (let [[vx vy vw vh] *viewport*
        [sw sh] *world-dims*
        twpx (/ sw vw)
        thpx (/ sh vh)]
    
    [(* (- x vx) twpx)
     (* (- y vy) thpx)
     (* w twpx)
     (* h thpx)]))

(defn viewport-offset
  ([] (let [[x y _ _] *viewport*]
        [x y]))
  ([[x y]] (let [[_ _ w h] *viewport*]
             (set! *viewport* [x y w h]))))

(defn viewport-dims
  ([] (let [[_ _ w h] *viewport*]
        [w h]))
  ([[w h]] (let [[x y _ _] *viewport*]
             (set! *viewport* [x y w h]))))

(defn context
  ([] (context *display*))
  ([canvas] (.getContext canvas "2d")))

(defn clear
  ([] (clear *display*))
  ([canvas]
     (let [w (.-width canvas)
           h (.-height canvas)
           ctx (context canvas)]
       (.clearRect ctx 0 0 w h))))

(def format string/format)

(defn color [r g b & [a]]
  (if (= a nil)
    (format "rgb(%f,%f,%f)" r g b)
    (format "rgba(%f,%f,%f,%f)" r g b a)))

(defn filled-rect [ctx [x y] [w h] color]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx x y w h))

(defn with-img [url callback]
  (let [img (js/Image.)]
    (set! (.-onload img) (fn [] (callback img)))
    (set! (.-src img) url)))

(defn img-dims [img]
  [(.-width img) (.-height img)])

(defn get-pixel-data [img]
  (let [canvas (dom/createDom "canvas")
        [w h] (img-dims img)]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    (doto (context canvas)
      (.drawImage img 0 0))
    {:data (-> (context canvas) (.getImageData 0 0 w h) (.-data))
     :dims [w h]}))

(defn get-pixel-idx [pdata idx]
  (let [data (:data pdata)
        r (aget data idx)
        g (aget data (+ idx 1))
         b (aget data (+ idx 2))
        a (aget data (+ idx 3))]
    [r g b]))

(defn get-pixel [pdata x y]
  (let [[w h] (:dims pdata)
        idx (+ (* x 4) (* w y 4))]
    (if (and (>= x 0) (>= y 0) (< x w) (< y h))
      (get-pixel-idx pdata idx)
      *default-color*)))

(defn load-map [img]
  {:pdata (get-pixel-data img)
   :dims (img-dims img)})

(defn draw-map [map]
  (let [pdata (:pdata map)
        [w h] (:dims map)
        [vw vh] (viewport-dims)
        [vx vy] (viewport-offset)
        ctx (context)]

    (doall
     (for [x (range vw)
           y (range vh)]
       (let [rx (+ x vx)
             ry (+ y vy)
             [tx ty tw th] (transform [rx ry 1 1])]
         (filled-rect ctx [tx ty] [tw th]
                      (apply color (get-pixel pdata rx ry))))))))

(def *current-map* nil)

(def +map-symbols+
  {[255 255 255] :nothing
   [255 0     0] :boundary
   [0   0   255] :platform})

(def *command-state-map* {})

(defn- keydown [e]
  (set! *command-state-map* (conj *command-state-map* {(.-keyCode e) true})))

(defn- keyup [e]
  (set! *command-state-map* (disj *command-state-map* (.-keyCode e))))

(defn prepare-input []
  (let [doc js/document]
    (set! (.-keydown doc) keydown)
    (set! (.-keyup doc) keyup)))

(defn until-false [callback timeout]
  (js/goog.Timer.callOnce
   (fn []
     (when (callback)
       (until-false callback timeout)))
   timeout))

(defn draw []
  (clear)
  (draw-map *current-map*))

(def *last-time* (js/goog.now))
(def +ticks-per-ms+ (/ 10 100))

(defn cycle []
  (let [now (js/goog.now)
        dtms (- now *last-time*)
        ticks (js/Math.floor (* +ticks-per-ms+ dtms))]
    (js/alert (format "%f ticks have elapsed" ticks))))

(defn tick [])
(defn ^:export main []
  (dom/setTextContent (content) "")
  (prepare-display)

  (with-img "maps/test.png"
    (fn [img]
      (let [map (load-map img)]
        (draw-map map)
        (doto (gevents/KeyHandler. js/document)
          (event/listen
           "key"
           (fn [e]
             (let [[vx vy] (viewport-offset)
                   code (.-keyCode e)]
               (cond
                 (= code (.-LEFT gevents/KeyCodes))
                 (viewport-offset [(- vx 1) vy])
                 
                 (= code (.-RIGHT gevents/KeyCodes))
                 (viewport-offset [(+ vx 1) vy])
                 
                 (= code (.-UP gevents/KeyCodes))
                 (viewport-offset [vx (- vy 1)])
                 
                 (= code (.-DOWN gevents/KeyCodes))
                 (viewport-offset [vx (+ vy 1)])))
             (clear)
             (draw-map map))))))))


