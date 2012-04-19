(ns showoff.showoff
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.string.format :as format)))

(defprotocol Tickable
  (tick [obj]))

(defprotocol Rectable
  (to-rect [obj]))

(defprotocol Drawable
  (draw [obj ctx]))

(extend-type default
  Drawable
  (draw [_ _] false))

(def +ticks-per-ms+ (/ 33 1000))
(def +secs-per-tick+ (/ (* +ticks-per-ms+ 1000)))

(def ^:dynamic *display* nil)
(def ^:dynamic *viewport* nil)
(def ^:dynamic *world-dims* [640 480]) ;; expressed in pixels
(def ^:dynamic *tile-dims* [8 8])
(def *tile-in-world-dims* [(/ 640 16) (/ 480 12)])

(def *default-color* [255 0 255])
(def *entities* (atom #{}))

(defn make-canvas [[w h]]
  (let [canvas (dom/createDom "canvas")]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    canvas))

(defn viewport-rect []
  (to-rect *viewport*))

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

(defn resize-nearest-neighbor
  ([img dest-dims]
     (let [[w h] (img-dims img)]
       (resize-nearest-neighbor img [0 0 w h] dest-dims)))
  
  ([img src-rect dest-dims]
     (let [[sx sy sw sh] src-rect
           [nw nh] dest-dims
           pdata (get-pixel-data img)
           rw (/ nw sw)
           rh (/ nh sh)
           canvas (make-canvas dest-dims)
           ctx (context canvas)]
       
       (doseq [x (range sw)
               y (range sh)]
         (let [px (+ x sx)
               py (+ y sy)
               pix (get-pixel pdata px py)]
           (set! (.-fillStyle ctx) (color pix (get-pixel-alpha pdata px py)))
           (.fillRect ctx (* x rw) (* y rh) rw rh)))

       canvas)))

;;; maps

(defn load-map [img map-symbols]
  (let [[w h] (img-dims img)
        dims [w h]
        pdata (get-pixel-data img)
        data (into
           []
           (for [idx (range (* w h))]
             (conj (map-symbols (get-pixel-idx pdata (* 4 idx)))
                   {:objects (atom #{})})))]
    
   {:dims dims
    :data data}))

(defn get-map-idx [map idx]
  (let [data (:data map)]
    (nth data idx)))

(declare rect->idxs)

(defn idx->coords [map idx]
  (let [[mw _] (:dims map)]
    [(mod idx mw)
     (Math/floor (/ idx mw))]))

(defn draw-map [map]
  (let [[mw _] (:dims map)
        ctx (context)]

    (doseq [idx (rect->idxs map (viewport-rect))]
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

(defn move-object [map obj src-idxs dest-idxs]
  ;; remove from old locations
  (doseq [idx src-idxs]
    (let [rec (get-map-idx map idx)
          objects (:objects rec)]
      (reset! objects (disj @objects obj))))

  ;; insert into new locations
  (doseq [idx dest-idxs]
    (let [rec (get-map-idx map idx)
          objects (:objects rec)]
      (reset! objects (conj @objects obj)))))

;;; vectors

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


;;; rects

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

(defn rect-offset [[rx ry rw rh] [ox oy]]
  [(+ rx ox) (+ ry oy) rw rh])

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

(defn idx->rect [map idx]
  (let [[mw _] (:dims map)]
    [(mod idx mw)
     (Math/floor (/ idx mw))
     1
     1]))

;;; collisions

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

(defn idxrect-contact [map idx rect]
  (rectrect-contact rect (idx->rect map idx)))

(defn supported-by-map [map rect]
  (let [[rx ry rw rh] rect
        half-width (* 0.5 rw)
        maxy (+ ry rh)
        test-rect [(+ rx (* 0.5 half-width)) maxy half-width 0.3]]
    (not (empty? (map-collisions map test-rect)))))

;;; particles

;; a force generator function produces a function that takes a
;; particle and returns a force
(defn drag-force-generator [drag-coefficient]
  (fn [p]
    (let [vel (:velocity p)
          mag-velocity (vec-mag vel)
          drag-dir (vec-negate (vec-unit vel))]
      (vec-scale drag-dir (* mag-velocity drag-coefficient)))))

(defn gravity-force-generator [g]
  (fn [p]
    [0 (* (:mass p) g)]))

(defn spring-force [displacement max-displacement spring-constant]
  (let [mag-displacement (vec-mag displacement)
        diff (- mag-displacement max-displacement)]
    (if (> mag-displacement max-displacement)
      (vec-scale (vec-unit displacement) (* spring-constant diff))
      [0 0])))

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
             :velocity vel
             :last-forces force})))

(defn max-incursion-contact [contacts]
  (reduce #(if (> (:incursion %1) (:incursion %2))
             %1
             %2)
          contacts))

(defn apply-particle-vs-map [p map rect restitution]
  (let [contacts (for [idx (map-collisions map rect)]
                   (idxrect-contact map idx rect))
        temp (into [] contacts)]
    (if (empty? contacts)
      p
      (let [max-contact (max-incursion-contact contacts)
            vel (:velocity p)
            vel-due-to-force (vec-scale (:last-forces p) (* (:mass p) +secs-per-tick+))
            veldiff (vec-mag (vec-sub vel vel-due-to-force))
            pos (:position p)
            newpos (vec-add pos (vec-scale (:normal max-contact)
                                           (+ 0.001 (:incursion max-contact))))
            restitution (if (< veldiff 0.06)
                          ;; our motion is only due to forces accumulated in 1
                          ;; frame. we should be resting relative to this contact
                          0
          
                          ;; we have a real constraint violation,
                          ;; resolve it using our usual restitution
                          restitution)
            newvel (vec-add vel (vec-scale (:normal max-contact)
                                           (* (+ 1 restitution)
                                              (Math/abs
                                               (vec-dot (:normal max-contact)
                                                        vel)))))]
        
        (conj p {:position newpos
                 :velocity newvel})))))



;;; entities

(defn tick-entities []
  (doseq [entity @*entities*]
    (tick entity)))

(defn draw-entities []
  (let [ctx (context)]
    (doseq [entity @*entities*]
      (draw entity ctx))))

(defn add-entity [map e]
  (swap! *entities* conj e)
  (when (and map (satisfies? Rectable e))
    (move-object map e nil (rect->idxs map (to-rect e)))))

(defn remove-entity [map e]
  (swap! *entities* disj e)
  (when (and map (satisfies? Rectable e))
    (move-object map e (rect->idxs map (to-rect e)) nil)))

;;; game loop

(def ^:private *last-time* (goog/now))
(def ^:private *remaining-time* 0)

(defn cycle [draw-world]
  (let [now (goog/now)
        dtms (+ (- now *last-time*) *remaining-time*)
        ticks (Math/floor (* +ticks-per-ms+ dtms))
        leftover (- dtms (/ ticks +ticks-per-ms+))]
    
    (set! *last-time* now)
    (set! *remaining-time* leftover)
    
    (loop [ii 0]
      (tick-entities)
      (when (< ii ticks)
        (recur (inc ii))))

    ;; only draw if we actually did something
    (when (> ticks 0)
      (draw-world))

    true))

