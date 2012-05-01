(ns showoff.showoff
  (:require [goog.string :as string]
            [goog.string.format :as format]
            [showoff.vec :as vec]
            [showoff.rect :as rect]
            [showoff.gfx :as gfx]
            [showoff.map :as map]))

(defprotocol Tickable
  (tick [obj]))

(defprotocol Rectable
  (to-rect [obj]))

(defprotocol Drawable
  (draw [obj ctx]))

(extend-type default
  Drawable
  (draw [_ _] false)

  Tickable
  (tick [_] nil))

(def +ticks-per-ms+ (/ 35 1000))
(def +secs-per-tick+ (/ (* +ticks-per-ms+ 1000)))

(def ^:dynamic *display* nil)
(def ^:dynamic *viewport-function* nil)
(def ^:dynamic *world-dims* [640 480]) ;; expressed in pixels
(def *tile-in-world-dims* [(/ 640 16) (/ 480 12)])
(def *tile-dims* [16 16])

(def *default-color* [255 0 255])
(def *entities* (atom #{}))

(defn viewport-rect []
  (*viewport-function*))

(defn set-display-and-viewport [display display-size viewport-function]
  (set! *display* display)
  (set! *viewport-function* viewport-function)
  (set! *world-dims* display-size)
  (let [[_ _ vw vh] (viewport-rect)
        [sw sh] display-size]
    (set! *tile-in-world-dims* [(/ sw vw) (/ sh vh)])))

(defn display []
  *display*)

(defn transform [[x y w h]]
  "convert from x y w h in tilespace to a position in visible screen
space taking into account the current viewport"
  (let [[vx vy vw vh] (viewport-rect)
        [sw sh] *world-dims*
        [twpx thpx] *tile-in-world-dims*]
    
    [(Math/floor (* (- x vx) twpx))
     (Math/floor (* (- y vy) thpx))
     (Math/floor (* w twpx))
     (Math/floor (* h thpx))]))

(defn with-transform [ctx viewport callback]
  (.save ctx)
  (let [[vx vy] viewport
        [tw th] *tile-in-world-dims*]
    (.scale ctx tw th)
    (.translate ctx (- vx) (- vy))
    (callback))
  (.restore ctx))

(def format string/format)

(defn color
  ([[r g b]]
     (str "rgb(" r "," g "," b ")"))

  ([[r g b] a]
     (str "rgba(" r "," g "," b "," a ")")))

(defn fill-style [ctx color]
  (set! (.-fillStyle ctx) color))

(defn filled-rect [ctx [x y w h] color]
  (let [[tx ty tw th] (transform [x y w h])]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx tx ty tw th)))

(defn draw-sprite [ctx img [x y]]
  (let [[tx ty] (transform [x y 1 1])]
    (.drawImage ctx img (Math/floor tx) (Math/floor ty))))

(defn resize-nearest-neighbor
  ([pdata dest-dims]
     (let [[w h] (:dims pdata)]
       (resize-nearest-neighbor pdata [0 0 w h] dest-dims)))
  
  ([pdata src-rect dest-dims]
     (gfx/map-nearest-neighbor pdata src-rect dest-dims gfx/pixel-identity)))

;;; fonts

(defn extract-char-images [img char-dims num-chars scale color]
  (let [[iw ih] (gfx/img-dims img)
        [cw ch] char-dims
        new-dims [(* cw scale) (* ch scale)]
        chars-in-row (/ iw cw)
        [r g b] color
        pdata (gfx/get-pixel-data img)]
    (for [idx (range num-chars)]
      (let [oy (* ch (Math/floor (/ idx chars-in-row)))
            ox (* cw (mod idx chars-in-row))
            char-rect [ox oy cw ch]]
        (gfx/map-nearest-neighbor
         pdata char-rect new-dims
         (fn [dest-data dest-base src-data src-base]
           (when (= 0 (aget src-data src-base))
             (aset dest-data dest-base r)
             (aset dest-data (+ 1 dest-base) g)
             (aset dest-data (+ 2 dest-base) b)
             (aset dest-data (+ 3 dest-base) 255))))))))

(defn load-font [img characters char-dims scale color]
  (let [characters (.toUpperCase characters)
        [cw ch] char-dims
        images (extract-char-images img char-dims (count characters) scale color)
        font (zipmap characters images)
        font (conj font {:dims [(* cw scale) (* ch scale)]})]
    font))

(defn with-loaded-font [path characters char-dims scale color callback]
  (gfx/with-img path
    (fn [img]
      (callback (load-font img characters char-dims scale color)))))

(defn draw-text [ctx font chrs pos]
  (let [chrs (.toUpperCase chrs)
        [ox oy] pos
        [cw ch] (or (:dims font) [16 16])]
    (dotimes [idx (count chrs)]
      (let [ch (nth chrs idx)
            img (font ch)]
        (if img
          (.drawImage ctx img (+ ox (* cw idx)) oy)
          (do
            (set! (.-fillStyle ctx) (color *default-color*))
            (.fillRect ctx (+ ox (* cw idx)) oy cw ch)))))))

(defn draw-text-centered [ctx font chrs pos]
  (let [[cw ch] (or (:doms font) [16 16])
        width (* cw (count chrs))
        [px py] pos
        x (- px (/ width 2))
        y (- py (/ ch 2))]
    (draw-text ctx font chrs [x y])))

;;; collisions

(defmulti record-vs-rect #(:shape %1))

(defn map-collisions [map rect]
  (filter
   (fn [res] (not (nil? res)))
   (for [idx (map/rect->idxs map rect)]
     (let [rec (map/get-map-idx map idx)]
       (cond
         ;; not solid
         (not (:collidable rec))
         nil
         
         ;; the easy case, a square shape
         (and (:collidable rec) (= (:shape rec) :rect))
         idx

         ;; the more costly case... just something else
         (not (nil? (record-vs-rect rec rect)))
         idx

         :else
         nil)))))

(defn move-check-map-collision [map [dx dy] rect on-x-collide on-y-collide]
  (let [x-moved-rect (rect/offset rect [dx 0])
        x-offset (if-let [hit-idx (first (map-collisions map x-moved-rect))]
                   (do (on-x-collide hit-idx) 0)
                   dx)

        y-moved-rect (rect/offset rect [x-offset dy])
        y-offset (if-let [hit-idx (first (map-collisions map y-moved-rect))]
                   (do (on-y-collide hit-idx) 0)
                   dy)]
    
    [x-offset y-offset]))

(defmethod record-vs-rect :rect
  [rec rect]
  (let [[mx my] (:coords rec)
        map-rect [mx my 1 1]]
    (when (rect/intersect map-rect rect)
      (rect/contact rect map-rect))))

(def ^:private sqrt2 (Math/sqrt 2))

(def ^:private +corner-fuzz+ 0.1)

(defmethod record-vs-rect :right-triangle
  [rec rect]
  (let [[mx my] (:coords rec)
        [rx ry rw rh] rect
        {:keys [slope intercept top-filled]} rec
        corner-test (fn [x y]
                      (when (and (> x (- mx +corner-fuzz+))
                                 (< x (+ mx 1 +corner-fuzz+))
                                 (> y (- my +corner-fuzz+))
                                 (< y (+ my 1 +corner-fuzz+)))
                        (let [dx (- x mx)
                              dy (- y my)
                              ly (+ intercept (* slope dx))]
                          (cond
                            (and top-filled (< dy ly))
                            (* sqrt2 (- ly dy))
                            
                            (and (not top-filled) (> dy ly))
                            (* sqrt2 (- dy ly))
                            
                            :else
                            false))))] 
    
    (let [tl (corner-test rx ry)
          tr (corner-test (+ rx rw) ry)
          bl (corner-test rx (+ ry rh))
          br (corner-test (+ rx rw) (+ ry rh))]
      (cond
        tl {:normal (vec/unit [1 1]) :incursion tl}
        tr {:normal (vec/unit [-1 1]) :incursion tr}
        bl {:normal (vec/unit [1 -1]) :incursion bl}
        br {:normal (vec/unit [-1 -1]) :incursion br}
        :else nil))))

(defn idxrect-contact [map idx rect]
  (record-vs-rect (map/get-map-idx map idx) rect))

(defn supported-by-map [map rect]
  (let [[rx ry rw rh] rect
        half-width (* 0.5 rw)
        maxy (+ ry rh)
        test-rect [(+ rx (* 0.5 half-width)) maxy half-width 0.1]]
    (not (empty? (map-collisions map test-rect)))))

(defn head-bumped-map [map rect]
  (let [[rx ry rw rh] rect
        half-width (* 0.5 rw)
        test-rect [(+ rx (* 0.5 half-width)) ry half-width 0.1]]
    (not (empty? (map-collisions map test-rect)))))

;;; particles

;; a force generator function produces a function that takes a
;; particle and returns a force
(defn drag-force-generator [drag-coefficient]
  (fn [p]
    (let [vel (:velocity p)
          mag-velocity (vec/mag vel)
          drag-dir (vec/negate (vec/unit vel))]
      (vec/scale drag-dir (* mag-velocity mag-velocity drag-coefficient)))))

(defn gravity-force-generator [g]
  (fn [p]
    [0 (* (:mass p) g)]))

(defn spring-force [displacement max-displacement spring-constant]
  (let [mag-displacement (vec/mag displacement)
        diff (- mag-displacement max-displacement)]
    (if (> mag-displacement max-displacement)
      (vec/scale (vec/unit displacement) (* spring-constant diff))
      [0 0])))

(defn accumulate-from-generators [p generators initial]
  (reduce (fn [result func] (vec/add result (func p)))
          initial
          generators))

(defn round-to-pixel [[vx vy]]
  (let [[tw th] *tile-in-world-dims*]
    [(/ (Math/round (* vx tw 2)) tw 2) (/ (Math/round (* vy th 2)) th 2)]))

(defn sum-overriding-in-direction [a b]
  "sum b into a but a . bhat = |b|"
  (let [bdir (vec/unit b)
        a-in-bdir (vec/dot a bdir)
        a-without-bdir (vec/sub a (vec/scale bdir a-in-bdir))]
    (vec/add a-without-bdir b)))

(defn integrate-particle [p]
  (let [force (accumulate-from-generators p (:force-generators p) [0 0])
        ;; velocity generators should override whatever is going on in
        ;; the direction they produce data
        generated-vel (accumulate-from-generators p (:velocity-generators p) [0 0])
        base-vel (if (= generated-vel [0 0])
                   (:velocity p)
                   (sum-overriding-in-direction (:velocity p) generated-vel))
        
        base-pos (:position p)
        acc (vec/scale force (/ (:mass p)))
        last-acc (vec/scale (or (:last-forces p) [0 0]) (/ (:mass p)))
        pos (vec/add
             base-pos
             (vec/add (vec/scale base-vel +secs-per-tick+)
                      (vec/scale last-acc (* 0.5 +secs-per-tick+ +secs-per-tick+))))
        vel (vec/add
             base-vel
             (vec/scale (vec/add last-acc acc) (/ +secs-per-tick+ 2)))]
    
    (conj p {:position pos
             :velocity vel
             :last-forces force})))

(defn max-incursion-contact [contacts]
  (reduce #(if (> (:incursion %1) (:incursion %2))
             %1
             %2)
          contacts))

(defn apply-particle-vs-map [p map rect restitution]
  (let [contacts (for [idx (map-collisions map rect)]
                   (rect/contact (map/idx->rect map idx) rect))]
    (if (empty? contacts)
      p
      (let [{:keys [normal incursion]} (max-incursion-contact contacts)
            vel (:velocity p)
            vel-due-to-force (vec/scale (:last-forces p)
                                        (* (/ (:mass p)) +secs-per-tick+))
            veldiff (vec/mag (vec/sub vel vel-due-to-force))
            pos (:position p)
            newpos (vec/add pos (vec/scale normal incursion))
            newvel (vec/add vel (vec/scale normal
                                 (* (+ 1 restitution)
                                    (Math/abs (vec/dot normal vel)))))]
        
        (conj p {:position newpos
                 :velocity newvel})))))



;;; entities

(defn tick-entities []
  (doseq [entity @*entities*]
    (tick entity)))

(defn draw-entities [ctx]
  (doseq [entity @*entities*]
    (draw entity ctx)))

(defn add-entity [map e]
  (swap! *entities* conj e)
  (when (and map (satisfies? Rectable e))
    (map/move-object map e nil (map/rect->idxs map (to-rect e)))))

(defn remove-entity [map e]
  (swap! *entities* disj e)
  (when (and map (satisfies? Rectable e))
    (map/move-object map e (map/rect->idxs map (to-rect e)) nil)))

(defn clear-entities [map]
  (doseq [e @*entities*]
    (remove-entity map e)))

;;; game loop

(def ^:private *last-time* (goog/now))
(def ^:private *remaining-ticks* 0)
(def ^:private *last-ticks-evaled* 0)
(def ^:private *last-ticks-time* 0)
(def ^:private *last-draw-time* 0)

(defn reset-tick-clock []
  (set! *last-time* (goog/now)))

(defn stats-string []
  (format "%3d ms/tick %3d ms/draw"
          (Math/floor (/ *last-ticks-time* *last-ticks-evaled*))
          *last-draw-time*))

(defn cycle-once [after-ticks]
  (let [now (goog/now)
        elapsed-time (- now *last-time*)
        remaining-ticks (+ *remaining-ticks* (* elapsed-time +ticks-per-ms+))
        ticks (Math/floor remaining-ticks)
        remaining-ticks (- remaining-ticks ticks)]
    
    (set! *last-time* now)

    ;; we assume delays in excess of 2 seconds mean that the browser
    ;; stopped calling us because we were in the background. we don't
    ;; tick or draw in this case but wait for the next refresh.
    (when (< elapsed-time 2000)
      (set! *remaining-ticks* remaining-ticks)
      
      (let [start (goog/now)]
        (dotimes [ii ticks]
          (tick-entities))
        (set! *last-ticks-time* (- (goog/now) start))
        (set! *last-ticks-evaled* ticks)))

    (let [start (goog/now)
          result (after-ticks ticks)]
      (set! *last-draw-time* (- (goog/now) start))
      result)))

