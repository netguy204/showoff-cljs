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
(def *tile-in-world-dims* [(/ 640 16) (/ 480 12)])

(def *default-color* [255 0 255])

(defn make-canvas [[w h]]
  (let [canvas (dom/createDom "canvas")]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    canvas))

(defn prepare-display []
  (let [[w h] *world-dims*]
    (set! *display* (make-canvas [w h]))
    (set! *viewport* [0 0 16 12])
    (dom/appendChild (content) *display*)))

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
  ([canvas] (let [ctx (.getContext canvas "2d")]
              (set! (.-imageSmoothingEnabled ctx) false)
              ctx)))

(defn clear
  ([] (clear *display*))
  ([canvas]
     (let [w (.-width canvas)
           h (.-height canvas)
           ctx (context canvas)]
       (.clearRect ctx 0 0 w h))))

(def format string/format)

(defn color
  ([[r g b]]
     (format "rgb(%f,%f,%f)" r g b))

  ([[r g b] a]
     (format "rgba(%f,%f,%f,%f)" r g b a)))

(defn filled-rect [ctx [x y] [w h] color]
  (let [[tx ty tw th] (transform [x y w h])]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx tx ty tw th)))

(defn draw-sprite [ctx img [x y]]
  (let [[tx ty tw th] (transform [x y 1 1])
        [sw sh] (img-dims img)]
    (.drawImage ctx img
                0 0 sw sh
                tx ty tw th)))

(defn get-img [url]
  (let [img (js/Image.)]
    (set! (.-src img) url)
    img))

(defn with-img [url callback]
  (let [img (get-img url)]
    (set! (.-onload img) (fn [] (callback img)))
    img))

(defn img-dims [img]
  [(.-width img) (.-height img)])

(defn img->canvas [img]
  (let [[w h] (img-dims img)
        canvas (make-canvas [w h])]
    (.drawImage (context canvas) img 0 0)
    canvas))

(defn get-pixel-data [img]
  (let [canvas (img->canvas img)
        [w h] (img-dims img)]
    {:data (-> (context canvas) (.getImageData 0 0 w h) (.-data))
     :dims [w h]}))

(defn get-pixel-idx [pdata idx]
  (let [data (:data pdata)
        r (aget data idx)
        g (aget data (+ idx 1))
        b (aget data (+ idx 2))]
    [r g b]))

(defn get-pixel [pdata x y]
  (let [[w h] (:dims pdata)
        idx (+ (* x 4) (* w y 4))]
    (if (and (>= x 0) (>= y 0) (< x w) (< y h))
      (get-pixel-idx pdata idx)
      *default-color*)))

(defn get-pixel-alpha [pdata x y]
  (let [[w h] (:dims pdata)
        data (:data pdata)
        idx (+ (* x 4) (* w y 4))]
    (aget data (+ idx 3))))

(defn resize-nearest-neighbor [img dims]
  (let [[nw nh] dims
        pdata (get-pixel-data img)
        [iw ih] (:dims pdata)
        rw (/ nw iw)
        rh (/ nh ih)
        canvas (make-canvas dims)
        ctx (context canvas)]
    
    (doseq [x (range iw)
            y (range ih)]
      (let [pix (get-pixel pdata x y)]
        (set! (.-fillStyle ctx) (color pix (get-pixel-alpha pdata x y)))
        (.fillRect ctx (* x rw) (* y rh) rw rh)))

    canvas))

(def +map-symbols+ nil) ;; with-prepare-assets will fill this in

(defn load-map [img]
  (let [[w h] (img-dims img)
        dims [w h]
        pdata (get-pixel-data img)
        data (into
           []
           (for [idx (range (* w h))]
             (+map-symbols+ (get-pixel-idx pdata (* 4 idx)))))]
    
   {:pdata pdata
    :dims dims
    :data data}))

(defn get-map-data [map x y]
  (let [[w h] (:dims map)
        data (:data map)]
    (when (and (>= x 0) (>= y 0) (< x w) (< y h))
      (nth data (+ x (* y w))))))

(defn draw-map [map]
  (let [[vx vy vw vh] *viewport*
        ctx (context)]

    (doseq [x (range vw)
            y (range vh)]
      (let [rx (+ x vx)
            ry (+ y vy)
            [sw sh] *tile-dims*
            frx (Math/round rx)
            fry (Math/round ry)
            drx (- frx rx)
            dry (- fry ry)
            rec (get-map-data map frx fry)
            sx (+ rx drx)
            sy (+ ry dry)]
        (cond
          (nil? rec)
          (filled-rect ctx [sx sy] [1 1] (color *default-color*))
          
          (= (rec :kind) :rect)
          (filled-rect ctx [sx sy] [1 1]
                       (color (rec :color)))

          (= (rec :kind) :image)
          (draw-sprite ctx (:image rec) [sx sy]))))))

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
(def *guy-sprite* (get-img "sprites/guy.png"))
(def *hud-sprite* (get-img "hud/hud.png"))

(defn draw-hud []
  (let [[w h] (img-dims *hud-sprite*)
        [sw sh] *world-dims*]
    (.drawImage (context) *hud-sprite*
                0 0 w h
                0 0 sw sh)))
(defn draw []
  (clear)
  (draw-map *current-map*)
  (draw-sprite (context) *guy-sprite* *guy-position*)
  (draw-hud))

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
(def +viewport-spring-constant+ 3)
(def +viewport-drag-coefficient+ 1)
(def +viewport-mass+ 1)
(def +viewport-max-displacement+ 1)

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

(defn with-prepared-assets [callback]
  ;; a few assets we can resize lazily
  (with-img "hud/hud.png"
    (fn [hud]
      (set! *hud-sprite* (resize-nearest-neighbor hud *world-dims*))))

  (with-img "sprites/guy.png"
    (fn [guy]
      (set! *guy-sprite* (resize-nearest-neighbor guy *tile-in-world-dims*))))
  
  ;; its critical that +map-symbols+ be built before callback is
  ;; invoked
  (with-img "sprites/air.png"
    (fn [air]
      (with-img "sprites/dirt.png"
        (fn [dirt]
          (set! +map-symbols+
                {[255 255 255]
                 {:kind :image
                  :image (resize-nearest-neighbor air *tile-in-world-dims*)}
                 
                 [255 0     0]
                     {:kind :image
                      :image (resize-nearest-neighbor dirt *tile-in-world-dims*)}
                     
                     [0   0   255]
                     {:kind :rect
                      :color [0 0 255]}})
          (callback))))))

(defn ^:export main []
  (dom/setTextContent (content) "")
  (prepare-display)
  (prepare-input)
  
  (with-prepared-assets
    (fn []
      (with-img "maps/test.png"
        (fn [img]
          (set! *current-map* (load-map img))
          (until-false cycle (/ +ticks-per-ms+)))))))


