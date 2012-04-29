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

(defn make-canvas [[w h]]
  (let [canvas (dom/createDom "canvas")]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    canvas))

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
     (str "rgb(" r "," g "," b ")"))

  ([[r g b] a]
     (str "rgba(" r "," g "," b "," a ")")))

(defn fill-style [ctx color]
  (set! (.-fillStyle ctx) color))

(defn filled-rect [ctx [x y w h] color]
  (let [[tx ty tw th] (transform [x y w h])]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx tx ty tw th)))

(defn img-dims [img]
  [(.-width img) (.-height img)])

(defn draw-sprite [ctx img [x y]]
  (let [[tx ty] (transform [x y 1 1])
        [sw sh] (img-dims img)]
    (.drawImage ctx img (Math/floor tx) (Math/floor ty))))

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

(defn get-context-image-data [ctx [w h]]
  (.getImageData ctx 0 0 w h))

(defn get-canvas-data [canvas]
  (.-data (get-context-image-data (context canvas) (img-dims canvas))))

(defn get-pixel-data [img]
  (let [canvas (img->canvas img)
        [w h] (img-dims img)]
    {:data (get-canvas-data canvas)
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
  ([pdata dest-dims]
     (let [[w h] (:dims pdata)]
       (resize-nearest-neighbor pdata [0 0 w h] dest-dims)))
  
  ([pdata src-rect dest-dims]
     (let [[sx sy sw sh] src-rect
           [nw nh] dest-dims
           rw (/ nw sw)
           rh (/ nh sh)
           dest-pixel-count (* nw nh)
           canvas (make-canvas dest-dims)
           ctx (context canvas)
           src-data (:data pdata)
           [sfw _] (:dims pdata)
           dest-image-data (get-context-image-data ctx dest-dims)
           dest-data (.-data dest-image-data)]

       (dotimes [idx dest-pixel-count]
         (let [x (mod idx nw)
               y (Math/floor (/ idx nw))
               sxx (+ sx (Math/floor (/ x rw)))
               syy (+ sy (Math/floor (/ y rh)))
               dest-base (* idx 4)
               src-base (* 4 (+ sxx (* syy sfw)))]
           (aset dest-data dest-base (aget src-data src-base))
           (aset dest-data (+ 1 dest-base) (aget src-data (+ 1 src-base)))
           (aset dest-data (+ 2 dest-base) (aget src-data (+ 2 src-base)))
           (aset dest-data (+ 3 dest-base) (aget src-data (+ 3 src-base)))))
       (.putImageData ctx dest-image-data 0 0)
       canvas)))

;;; fonts

(defn extract-char-images [img char-dims num-chars scale color]
  (let [[iw ih] (img-dims img)
        [cw ch] char-dims
        pixels-per-char (* cw ch)
        fw (* cw scale)
        fh (* ch scale)
        chars-in-row (/ iw cw)
        pdata (get-pixel-data img)]
    (for [idx (range num-chars)]
      (let [oy (* ch (Math/floor (/ idx chars-in-row)))
            ox (* cw (mod idx chars-in-row))
            out (make-canvas [fw fh])
            ctx (context out)]
        (doseq [pxidx (range pixels-per-char)]
          (let [chy (Math/floor (/ pxidx cw))
                chx (mod pxidx cw)
                pix (get-pixel pdata (+ ox chx) (+ oy chy))]
            (when (= pix [0 0 0])
              (set! (.-fillStyle ctx) color)
              (.fillRect ctx (* chx scale) (* chy scale) scale scale))))
        out))))

(defn load-font [img characters char-dims scale color]
  (let [characters (.toUpperCase characters)
        [cw ch] char-dims
        images (extract-char-images img char-dims (count characters) scale color)
        font (zipmap characters images)
        font (conj font {:dims [(* cw scale) (* ch scale)]})]
    font))

(defn with-loaded-font [path characters char-dims scale color callback]
  (with-img path
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

;;; maps
(def *alerted-symbols* (atom #{}))

(defn load-map [img map-symbols]
  (let [[w h] (img-dims img)
        dims [w h]
        pdata (get-pixel-data img)
        data (into
           []
           (for [idx (range (* w h))]
             (let [pix (get-pixel-idx pdata (* 4 idx))
                   sym (map-symbols pix)]
               (if sym
                 (conj sym {:objects (atom #{})
                            :coords (idx->coords {:dims dims} idx)})
                 (when (not (@*alerted-symbols* pix))
                   (swap! *alerted-symbols* conj pix)
                   (js/alert (format "couldn't find map symbol %s" (pr-str pix))))))))]
    
   {:dims dims
    :data (apply array data)}))

(defn get-map-idx [map idx]
  (let [data (:data map)]
    (aget data idx)))

(defn set-map-idx [map idx newvalue]
  (aset (:data map) idx
        (merge newvalue {:objects (atom #{})
                         :coords (idx->coords map idx)})))

(declare rect->idxs)

(defn idx->coords [map idx]
  (let [[mw _] (:dims map)]
    [(mod idx mw)
     (Math/floor (/ idx mw))]))

(defn coords->idx [map [x y]]
  (let [[mw _] (:dims map)]
    (+ (Math/floor x) (* (Math/floor y) mw))))

(defn draw-map [map]
  (let [[mw mh] (:dims map)
        ctx (context)
        [ox oy ow oh] (viewport-rect)
        [tw th] *tile-in-world-dims*
        otx (Math/floor ox)
        oty (Math/floor oy)
        offx (- otx ox) ;; additive amount to fractionally offset tiles
        offy (- oty oy)
        rngx (- (Math/ceil (+ ox ow))
                (Math/floor ox))
        rngy (- (Math/ceil (+ oy oh))
                (Math/floor oy))
        num-idxs (* rngx rngy)
        map-data (:data map)]

    (dotimes [ii num-idxs]
      (let [viewrow (Math/floor (/ ii rngx))
            viewcol (mod ii rngx)
            maprow (+ viewrow oty)
            mapcol (+ viewcol otx)
            tx (+ viewcol offx)
            ty (+ viewrow offy)
            drawx (* tw tx)
            drawy (* th ty)
            mapidx (+ mapcol (* maprow mw))]
        (if (and (>= maprow 0) (>= mapcol 0)
                 (< maprow mh) (< mapcol mw)
                 (= ((aget map-data mapidx) :kind) :image))
          (.drawImage ctx ((aget map-data mapidx) :image) (Math/floor drawx) (Math/floor drawy)))))))

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

(defn rect-width [[_ _ w _]]
  w)

(defn rect-height [[_ _ _ h]]
  h)

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

(defmulti record-vs-rect #(:shape %1))

(defn map-collisions [map rect]
  (filter
   (fn [res] (not (nil? res)))
   (for [idx (rect->idxs map rect)]
     (let [rec (get-map-idx map idx)]
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

(defmethod record-vs-rect :rect
  [rec rect]
  (let [[mx my] (:coords rec)
        map-rect [mx my 1 1]]
    (when (rect-intersect map-rect rect)
      (rectrect-contact rect map-rect))))

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
        tl {:normal (vec-unit [1 1]) :incursion tl}
        tr {:normal (vec-unit [-1 1]) :incursion tr}
        bl {:normal (vec-unit [1 -1]) :incursion bl}
        br {:normal (vec-unit [-1 -1]) :incursion br}
        :else nil))))

(defn idxrect-contact [map idx rect]
  (record-vs-rect (get-map-idx map idx) rect))

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
          mag-velocity (vec-mag vel)
          drag-dir (vec-negate (vec-unit vel))]
      (vec-scale drag-dir (* mag-velocity mag-velocity drag-coefficient)))))

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

(defn round-to-pixel [[vx vy]]
  (let [[tw th] *tile-in-world-dims*]
    [(/ (Math/round (* vx tw 2)) tw 2) (/ (Math/round (* vy th 2)) th 2)]))

(defn sum-overriding-in-direction [a b]
  "sum b into a but a . bhat = |b|"
  (let [bdir (vec-unit b)
        a-in-bdir (vec-dot a bdir)
        a-without-bdir (vec-sub a (vec-scale bdir a-in-bdir))]
    (vec-add a-without-bdir b)))

(defn integrate-particle [p]
  (let [force (accumulate-from-generators p (:force-generators p) [0 0])
        ;; velocity generators should override whatever is going on in
        ;; the direction they produce data
        generated-vel (accumulate-from-generators p (:velocity-generators p) [0 0])
        base-vel (if (= generated-vel [0 0])
                   (:velocity p)
                   (sum-overriding-in-direction (:velocity p) generated-vel))
        
        base-pos (:position p)
        acc (vec-scale force (/ (:mass p)))
        last-acc (vec-scale (or (:last-forces p) [0 0]) (/ (:mass p)))
        pos (vec-add
             base-pos
             (vec-add (vec-scale base-vel +secs-per-tick+)
                      (vec-scale last-acc (* 0.5 +secs-per-tick+ +secs-per-tick+))))
        vel (vec-add
             base-vel
             (vec-scale (vec-add last-acc acc) (/ +secs-per-tick+ 2)))]
    
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
      (let [{:keys [normal incursion]} (max-incursion-contact contacts)
            vel (:velocity p)
            vel-due-to-force (vec-scale (:last-forces p)
                                        (* (/ (:mass p)) +secs-per-tick+))
            veldiff (vec-mag (vec-sub vel vel-due-to-force))
            pos (:position p)
            newpos (vec-add pos (vec-scale normal incursion))
            newvel (vec-add vel (vec-scale normal
                                 (* (+ 1 restitution)
                                    (Math/abs (vec-dot normal vel)))))]
        
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

