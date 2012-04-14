(ns showoff.core
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.string.format :as format)
            (goog.events :as gevents)
            (goog.Timer :as timer)
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
  (set! *viewport* [0 0 16 12])
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

(defn color [[r g b]]
  (format "rgb(%f,%f,%f)" r g b))

(defn filled-rect [ctx [x y] [w h] color]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx x y w h))

(defn with-img [url callback]
  (let [img (js/Image.)]
    (set! (.-onload img) (fn [] (callback img)))
    (set! (.-src img) url)
    img))

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

(def +map-symbols+
  {[255 255 255] {:kind :image
                  :image (with-img "sprites/air.png" identity)}
   [255 0     0] {:kind :image
                  :image (with-img "sprites/dirt.png" identity)}
   [0   0   255] {:kind :rect
                  :color [0 0 255]}})

(defn draw-sprite [ctx img pos]
  (let [[tx ty tw th] (transform [(nth pos 0) (nth pos 1) 1 1])
        [sw sh] *tile-dims*]
    (.drawImage ctx img
                0 0 sw sh
                tx ty tw th)))

(defn draw-map [map]
  (let [pdata (:pdata map)
        [w h] (:dims map)
        [vw vh] (viewport-dims)
        [vx vy] (viewport-offset)
        ctx (context)]

    (doseq [x (range vw)
            y (range vh)]
      (let [rx (+ x vx)
            ry (+ y vy)
            [tx ty tw th] (transform [rx ry 1 1])
            [sw sh] *tile-dims*
            pix (get-pixel pdata (Math/floor rx) (Math/floor ry))
            rec (+map-symbols+ pix)]
        (cond
          (nil? rec)
          (filled-rect ctx [tx ty] [tw th] (color *default-color*))
          
          (= (rec :kind) :rect)
          (filled-rect ctx [tx ty] [tw th]
                       (color (rec :color)))

          (= (rec :kind) :image)
          (draw-sprite ctx (:image rec) [rx ry]))))))

(def *current-map* nil)

(def *command-state-map* #{})

(defn- keydown [e]
  (set! *command-state-map* (conj *command-state-map* (.-keyCode e))))

(defn- keyup [e]
  (set! *command-state-map* (disj *command-state-map* (.-keyCode e))))

(defn prepare-input []
  (let [doc js/document]
    (gevents/listen doc (.-KEYDOWN gevents/EventType) keydown)
    (gevents/listen doc (.-KEYUP gevents/EventType) keyup)))

(defn until-false [callback timeout]
  (timer/callOnce
   (fn []
     (when (callback)
       (until-false callback timeout)))
   timeout))

(def *last-time* (js/goog.now))
(def *remaining-time* 0)
(def +ticks-per-ms+ (/ 30 1000))
(def +secs-per-tick+ (/ 30))
(def *guy-position* [1 1])
(def *guy-sprite* (with-img "sprites/guy.png" identity))

(defn draw []
  (clear)
  (draw-map *current-map*)
  (draw-sprite (context) *guy-sprite* *guy-position*))

(defn cycle []
  (let [now (js/goog.now)
        dtms (+ (- now *last-time*) *remaining-time*)
        ticks (js/Math.floor (* +ticks-per-ms+ dtms))
        leftover (- dtms (/ ticks +ticks-per-ms+))]
    
    (set! *last-time* now)
    (set! *remaining-time* leftover)
    
    (loop [ii 0]
      (tick)
      (when (< ii ticks)
        (recur (inc ii))))
    (tick ticks)

    (draw)

    true))

(defn- update-viewport-keyboard []
  (let [[vx vy] (viewport-offset)
        csm *command-state-map*
        step 0.2]
    (when (csm (.-LEFT gevents/KeyCodes))
      (viewport-offset [(- vx step) vy]))

    (when (csm (.-RIGHT gevents/KeyCodes))
      (viewport-offset [(+ vx step) vy]))

    (when (csm (.-UP gevents/KeyCodes))
      (viewport-offset [vx (- vy step)]))

    (when (csm (.-DOWN gevents/KeyCodes))
      (viewport-offset [vx (+ vy step)]))))

(defn- update-guy-keyboard []
    (let [[x y] *guy-position*
        csm *command-state-map*
        step 0.05]
    (when (csm (.-LEFT gevents/KeyCodes))
      (set! *guy-position* [(- x step) y]))

    (when (csm (.-RIGHT gevents/KeyCodes))
      (set! *guy-position* [(+ x step) y]))

    (when (csm (.-UP gevents/KeyCodes))
      (set! *guy-position* [x (- y step)]))

    (when (csm (.-DOWN gevents/KeyCodes))
      (set! *guy-position* [x (+ y step)]))))

(def *viewport-velocity* [0 0])
(def +viewport-spring-constant+ 1)
(def +viewport-drag-coefficient+ 1)
(def +viewport-mass+ 1)
(def +viewport-max-displacement+ 3)

(defn vec-add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn vec-sub [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])

(defn vec-mag [[ax ay]]
  (Math/sqrt (+ (* ax ax) (* ay ay))))

(defn vec-scale [[ax ay] scale]
  [(* ax scale) (* ay scale)])

(defn vec-unit [v]
  (let [mag (vec-mag v)]
    (if (> mag 0)
      (vec-scale v (/ (vec-mag v)))
      [0 0])))

(defn vec-negate [[ax ay]]
  [(- ax) (- ay)])

(defn update-viewport-position []
  ;; try to keep the player character basically centered
  (let [[vx vy vw vh] *viewport*
        vc (vec-add [vx vy] (vec-scale [vw vh] 0.5))
        displacement (vec-sub *guy-position* vc)

        ;; compute the spring force vector
        mag-displacement (vec-mag displacement)
        spring-force-mag (if (> mag-displacement +viewport-max-displacement+)
                           (* (- mag-displacement +viewport-max-displacement+)
                              +viewport-spring-constant+)
                           0)
        spring-force (vec-scale (vec-unit displacement) spring-force-mag)

        ;; compute the drag force vector
        mag-velocity (vec-mag *viewport-velocity*)
        drag-force (vec-scale (vec-negate (vec-unit *viewport-velocity*))
                              (* mag-velocity +viewport-spring-constant+))
        
        force (vec-add spring-force drag-force)
        acc (vec-scale force (/ +viewport-mass+))
        vel (vec-add *viewport-velocity* (vec-scale acc +secs-per-tick+))
        pos (vec-add [vx vy] (vec-scale vel +secs-per-tick+))]
    
    (viewport-offset pos)
    (set! *viewport-velocity* vel)))

(defn tick []
  (update-guy-keyboard)
  (update-viewport-position))

(defn ^:export main []
  (dom/setTextContent (content) "")
  (prepare-display)
  (prepare-input)
  
  (with-img "maps/test.png"
    (fn [img]
      (set! *current-map* (load-map img))
      (until-false cycle (/ +ticks-per-ms+)))))


