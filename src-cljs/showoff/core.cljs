(ns showoff.core
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.string.format :as format)
            (goog.events :as gevents)
            (goog.Timer :as timer)
            (goog.events.KeyHandler :as geventskey)
            (clojure.browser.event :as event)
            (clojure.browser.repl :as repl)))

(defn by-id [id]
  (dom/getElement id))

(defn content []
  (by-id "content"))

(def ^:dynamic *display* nil)
(def ^:dynamic *viewport* [0 0 16 12]) ;; expressed in tiles
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

(defn img-dims [img]
  [(.-width img) (.-height img)])

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

(defn get-map-idx [map idx]
  (let [data (:data map)]
    (nth data idx)))

(defn get-map-data [map x y]
  (let [[w h] (:dims map)]
    (when (and (>= x 0) (>= y 0) (< x w) (< y h))
      (get-map-idx map (+ x (* y w))))))

(defn rect->idxs-all [map rect]
  (let [[rx ry rw rh] rect
        [mw mh] (:dims map)
        rngx (- (Math/ceil (+ rw rx))
                (Math/floor rx))
        rngy (- (Math/ceil (+ rh ry))
                (Math/floor ry))
        ox (Math/floor rx)
        oy (Math/floor ry)]

    (for [ix (range rngx)
          iy (range rngy)]
      (let [x (+ ox ix)
            y (+ oy iy)]
        (when (and (>= x 0) (>= y 0) (< x mw) (< y mh))
          (+ x (* y mw)))))))

(defn rect->idxs [map rect]
  (filter (fn [idx] (not (nil? idx))) (rect->idxs-all map rect)))

(defn idx->coords [map idx]
  (let [[mw _] (:dims map)]
    [(mod idx mw)
     (Math/floor (/ idx mw))]))

(defn draw-map [map]
  (let [[mw _] (:dims map)
        ctx (context)]

    (doseq [idx (rect->idxs map *viewport*)]
      (let [tx (mod idx mw)
            ty (Math/floor (/ idx mw))
            rec (get-map-idx map idx)]
        (cond
          (nil? rec)
          (filled-rect ctx [tx ty] [1 1] (color *default-color*))
          
          (= (rec :kind) :rect)
          (filled-rect ctx [tx ty] [1 1]
                       (color (rec :color)))
          
          (= (rec :kind) :image)
          (draw-sprite ctx (:image rec) [tx ty]))))))

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

(def *last-time* (goog/now))
(def *remaining-time* 0)
(def +ticks-per-ms+ (/ 30 1000))
(def +secs-per-tick+ (/ 30))
(def *guy-sprite* (get-img "sprites/guy.png"))
(def *hud-sprite* (get-img "hud/hud.png"))

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

(defn vec-dot [[ax ay] [bx by]]
  (+ (* ax bx) (* ay by)))

;; a force generator function produces a function that takes a
;; particle and returns a force
(defn drag-force-generator [drag-coefficient]
  (fn [p]
    (let [vel (:velocity p)
          mag-velocity (vec-mag vel)
          drag-dir (vec-negate (vec-unit vel))]
      (vec-scale drag-dir (* mag-velocity drag-coefficient)))))

(defn spring-force [displacement max-displacement spring-constant]
  (let [mag-displacement (vec-mag displacement)
        diff (- mag-displacement max-displacement)]
    (if (> mag-displacement max-displacement)
      (vec-scale (vec-unit displacement) (* spring-constant diff))
      [0 0])))

;; a velocity generator function produces a function that takes a
;; particle and returns a velocity
(defn keyboard-velocity-generator [keycode value]
  (fn [p]
    (if (*command-state-map* keycode)
      value
      [0 0])))

(defn rect-center [rect]
  (let [[vx vy vw vh] rect]
    (vec-add [vx vy] (vec-scale [vw vh] 0.5))))

(defn rect-minx [[ax _ _ _]]
  ax)

(defn rect-maxx [[ax _ aw _]]
  (+ ax aw))

(defn rect-miny [[_ ay _ _]]
  ay)

(defn rect-maxy [[_ ay _ ah]]
  (+ ay ah))

(defn rect-minmax [[ax ay aw ah]]
  [ax ay (+ ax aw) (+ ay ah)])

(defn rect-intersect [a b]
  (let [[aminx aminy amaxx amaxy] (rect-minmax a)
        [bminx bminy bmaxx bmaxy] (rect-minmax b)]
   (not
    (or (< amaxx bminx)
        (> aminx bmaxx)
        (< amaxy bminy)
        (> aminy bmaxy)))))

(def *viewport-particle*
  {:mass 1
   :position [0 0]
   :velocity [0 0]

   ;; try to keep the player character basically centered
   :force-generators
   [(fn [p] (spring-force (vec-sub (:position *guy-particle*) (rect-center *viewport*))
                          +viewport-max-displacement+
                          +viewport-spring-constant+))
    (drag-force-generator +viewport-drag-coefficient+)]})

(defn keyboard-direction-generators [scale]
  [(keyboard-velocity-generator (.-LEFT gevents/KeyCodes) [(- scale) 0])
   (keyboard-velocity-generator (.-RIGHT gevents/KeyCodes) [scale 0])
   (keyboard-velocity-generator (.-UP gevents/KeyCodes) [0 (- scale)])
   (keyboard-velocity-generator (.-DOWN gevents/KeyCodes) [0 scale])])

(def *guy-particle*
  {:mass 1
   :position [0 0]
   :velocity [0 0]

   ;; bring to a stop quickly
   :force-generators
   [(drag-force-generator 10)]

   :velocity-generators
   (keyboard-direction-generators 1)
   
   })

(defn guy-rect []
  (let [[x y] (:position *guy-particle*)]
    [(+ x 0.2) (+ y 0.1) 0.6 0.9]))

(defn accumulate-from-generators [p generators initial]
  (reduce (fn [result func] (vec-add result (func p)))
          initial
          generators))

(defn integrate-particle [p]
  (let [force (accumulate-from-generators p (:force-generators p) [0 0])
        base-vel (accumulate-from-generators p (:velocity-generators p) (:velocity p))
        base-pos (accumulate-from-generators p (:offset-generators p) (:position p))
        acc (vec-scale force (:mass p))
        vel (vec-add base-vel (vec-scale acc +secs-per-tick+))
        pos (vec-add base-pos (vec-scale vel +secs-per-tick+))]
    
    (conj p {:position pos
             :velocity vel})))

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
                 
                 [255 0 0]
                 {:kind :image
                  :image (resize-nearest-neighbor dirt *tile-in-world-dims*)
                  :collidable true}
                     
                 [0 0 255]
                 {:kind :rect
                  :color [0 0 255]}})
          (callback))))))

(defn map-collisions [map rect]
  (filter
   (fn [res] (not (nil? res)))
   (for [idx (rect->idxs map rect)]
     (let [rec (get-map-idx map idx)]
       (when (:collidable rec) idx)))))

(defn rectrect-horizontal-overlap [r1 r2]
  "assumes that there is some overlap"
  (let [[r1x _ r1w _] r1
        [r2x _ r2w _] r2]
    (- (min (+ r1x r1w)
            (+ r2x r2w))
       (max r1x r2x))))

(defn rectrect-vertical-overlap [r1 r2]
  "assuems that there is some overlap"
  (let [[_ r1y _ r1h] r1
        [_ r2y _ r2h] r2]
    (- (min (+ r1y r1h)
            (+ r2y r2h))
       (max r1y r2y))))

(defn rectrect-contact [r1 r2]
  "assumes that r1 and r2 intersect, normal will push r1 from r2"
  (let [ho (rectrect-horizontal-overlap r1 r2)
        vo (rectrect-vertical-overlap r1 r2)]
    (cond
      ;; horizontal violation
      (> vo ho)
      (if (< (rect-minx r1) (rect-minx r2))
        {:normal [-1 0]
         :incursion ho}
        {:normal [1 0]
         :incursion ho})

      ;; vertical violation
      (> ho vo)
      (if (< (rect-miny r1) (rect-miny r2))
        {:normal [0 -1]
         :incursion vo} 
        {:normal [0 1]
         :incursion vo})

      ;; corner-shot
      :else
      (cond
        ;; left side
        (< (rect-minx r1) (rect-minx r2))
        (if (< (rect-miny r1) (rect-maxy r2))
          ;; top-left corner
          {:normal (vec-unit [-1 -1])
           :incursion (vec-mag [ho vo])} 
          ;; bottom-left corner
          {:normal (vec-unit [-1 1])
           :incursion (vec-mag [ho vo])})

        ;; right side
        :else
        (if (< (rect-miny r1) (rect-maxy r2))
          ;; top-right corner
          {:normal (vec-unit [1 -1])
           :incursion (vec-mag [ho vo])} 
          ;; bottom-right corner
          {:normal (vec-unit [1 1])
           :incursion (vec-mag [ho vo])})))))

(defn update-viewport []
  (let [new-viewport (integrate-particle *viewport-particle*)]
    (viewport-offset (:position new-viewport))
    (set! *viewport-particle* new-viewport)))

(defn apply-particle-vs-map [p map rect restitution]
  (reduce
   (fn [p idx]
     (let [[tx ty] (idx->coords map idx)
           tilerect [tx ty 1 1]
           contact (rectrect-contact rect tilerect)
           pos (:position p)
           vel (:velocity p)
           newpos (vec-add pos (vec-scale (:normal contact) (:incursion contact)))
           newvel (vec-add vel (vec-scale (:normal contact) (* (+ 1 restitution)
                                                               (vec-dot (:normal contact)
                                                                        vel))))]
       (conj p {:position newpos
                :velocity vel})))
   p
   (map-collisions map rect)))

(defn update-guy []
  (set!
   *guy-particle*
   (apply-particle-vs-map (integrate-particle *guy-particle*)
                          *current-map*
                          (guy-rect)
                          0.3)))

(defn draw-hud []
  (let [[w h] (img-dims *hud-sprite*)
        [sw sh] *world-dims*]
    (.drawImage (context) *hud-sprite*
                0 0 w h
                0 0 sw sh)))

(defn draw-guy-tile-test []
  (let [ctx (context)]
    (doseq [idx (rect->idxs *current-map* (guy-rect))]
      (let [xy (idx->coords *current-map* idx)]
        (filled-rect ctx xy [1 1] (color [255 0 255]))))))

(defn draw-guy-collision-test []
  (doseq [idx (map-collisions *current-map* (guy-rect))]
    (filled-rect (context) (idx->coords *current-map* idx) [1 1] (color [255 0 255]))))

(defn draw []
  (clear)
  (draw-map *current-map*)
  (draw-guy-collision-test)
  (draw-sprite (context) *guy-sprite* (:position *guy-particle*))
  (draw-hud))

(defn tick []
  (update-guy)
  (update-viewport))

(defn cycle []
  (let [now (goog/now)
        dtms (+ (- now *last-time*) *remaining-time*)
        ticks (Math/floor (* +ticks-per-ms+ dtms))
        leftover (- dtms (/ ticks +ticks-per-ms+))]
    
    (set! *last-time* now)
    (set! *remaining-time* leftover)
    
    (loop [ii 0]
      (tick)
      (when (< ii ticks)
        (recur (inc ii))))

    (draw)

    true))

(defn ^:export main []
  (repl/connect "http://localhost:9000/repl")
  
  (dom/setTextContent (content) "")
  (prepare-display)
  (prepare-input)
  
  (with-prepared-assets
    (fn []
      (with-img "maps/test.png"
        (fn [img]
          (set! *current-map* (load-map img))
          (until-false cycle (/ +ticks-per-ms+)))))))


