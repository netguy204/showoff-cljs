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
(declare viewport-rect)
(def ^:dynamic *world-dims* [640 480]) ;; expressed in pixels
(def ^:dynamic *tile-dims* [8 8])
(def *tile-in-world-dims* [(/ 640 16) (/ 480 12)])

(def *default-color* [255 0 255])
(def *current-map* nil)
(def *command-state-map* #{})
(def *media-player* nil)
(def *entities* (atom #{}))

(defprotocol Tickable
  (tick [obj]))

(defprotocol Rectable
  (to-rect [obj]))

(defprotocol Drawable
  (draw [obj ctx]))

(extend-type default
  Drawable
  (draw [_ _] false))

(defn make-canvas [[w h]]
  (let [canvas (dom/createDom "canvas")]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    canvas))


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


(defn prepare-display []
  (let [[w h] *world-dims*]
    (set! *display* (make-canvas [w h]))
    (dom/appendChild (content) *display*)))

(defn prepare-sound []
  (let [spec {:resources ["music/epica.ogg"
                          "music/epica.mp3"]
              :autoplay "bg-music"
              :spritemap
              {:bg-music
               {:start 0.0
                :end 122
                :loop true}}}]
    (set! *media-player* (jukebox.Player. (clj->js spec)))))

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
             (conj (+map-symbols+ (get-pixel-idx pdata (* 4 idx)))
                   {:objects (atom #{})})))]
    
   {:dims dims
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

(defn idx->rect [map idx]
  (let [[mw _] (:dims map)]
    [(mod idx mw)
     (Math/floor (/ idx mw))
     1
     1]))

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

;; a velocity generator function produces a function that takes a
;; particle and returns a velocity
(defn keyboard-velocity-generator [keycode value]
  (fn [p]
    (if (*command-state-map* keycode)
      value
      [0 0])))

(defn keyboard-direction-generators [scale]
  [(keyboard-velocity-generator (.-LEFT gevents/KeyCodes) [(- scale) 0])
   (keyboard-velocity-generator (.-RIGHT gevents/KeyCodes) [scale 0])])

(defn supported-by-map [map rect]
  (let [[rx ry rw rh] rect
        half-width (* 0.5 rw)
        maxy (+ ry rh)
        test-rect [(+ rx (* 0.5 half-width)) maxy half-width 0.1]]
    (not (empty? (map-collisions map test-rect)))))

(defn jump-velocity-generator [rect-gen up-vel]
  (fn [p]
    (cond
      ;; not trying to jump
      (not (*command-state-map* (.-UP gevents/KeyCodes)))
      [0 0]

      ;; trying to jump and surface underfoot
      (supported-by-map *current-map* (rect-gen))
      [0 (- up-vel)]

      ;; can't jump, we're not supported
      :else
      [0 0])))

(defn ground-friction-generator [rect-gen coefficient]
  (fn [p]
    (cond
      ;; not applicable if we're not standing on something
      (not (supported-by-map *current-map* (rect-gen)))
      [0 0]

      ;; also not applicable if the player is trying to move
      (or (*command-state-map* (.-LEFT gevents/KeyCodes))
          (*command-state-map* (.-RIGHT gevents/KeyCodes)))
      [0 0]
      
      ;; we're supported by the map, introduce drag in the horizontal
      ;; direction.
      :else
      (let [xvel (vec-dot [1 0] (:velocity p))]
        (if (< (Math/abs xvel) 0.01)
          ;; stop us completely if we're not moving very fast
          (vec-scale [1 0] (- xvel)) ;; fixme: compute force needed to stop
          ;; otherwise slowdown by our factor
          (vec-scale [1 0] (* -1 coefficient xvel)))))))


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


(defn idxrect-contact [map idx rect]
  (rectrect-contact rect (idx->rect map idx)))

(defn apply-particle-vs-map [p map rect restitution]
  (let [contacts (for [idx (map-collisions map rect)]
                   (idxrect-contact map idx rect))
        temp (into [] contacts)]
    (if (empty? contacts)
      p
      (let [max-contact (reduce #(if (> (:incursion %1) (:incursion %2))
                                   %1
                                   %2)
                                contacts)
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


(defrecord Bullet [particle maxticks]
  Rectable
  (to-rect [bullet]
    (let [[x y] (:position @particle)]
      [x y 0.2 0.2]))
  
  Tickable
  (tick [bullet]
    (let [ticks (+ 1 (or (:ticks @particle) 0))]
      (cond
        ;; check for timeout
        (> ticks maxticks)
        (swap! *entities* disj bullet)

        ;; check for map collisions
        (not (empty? (map-collisions *current-map* (to-rect bullet))))
        (swap! *entities* disj bullet)
        
        ;; otherwise, integrate
        :else
        (reset! particle (conj (integrate-particle @particle)
                               {:ticks ticks})))))

  Drawable
  (draw [bullet ctx]
    (let [[x y w h] (to-rect bullet)]
      (filled-rect ctx [x y] [w h] (color [128 128 128])))))

(def *guy-extra-forces* (atom []))
(defn guy-extra-forces [p]
  (let [force (reduce vec-add [0 0] @*guy-extra-forces*)]
    (reset! *guy-extra-forces* [])
    force))

(defrecord Guy [particle]
  Rectable
  (to-rect [guy]
    (let [[x y] (:position @particle)]
      [(+ x 0.2) (+ y 0.1) 0.6 0.9]))

  Tickable
  (tick [guy]
    ;; firing?
    (when (*command-state-map* 32)
      (let [bullet {:mass 1
                    :position (vec-add [1 0.5] (:position @particle))
                    :velocity [6 0]}]
       (swap! *entities* conj (Bullet. (atom bullet) 40)))
      (swap! *guy-extra-forces* conj [-3 0]))
    
    (reset! particle (apply-particle-vs-map (integrate-particle @particle)
                                            *current-map*
                                            (to-rect guy)
                                            0.1)))

  Drawable
  (draw [guy ctx]
    (draw-sprite ctx *guy-sprite* (:position @particle))))

(def *guy*
  (Guy.
   (atom {:mass 1
          :position [2 2]
          :velocity [0 0]
          
          ;; bring to a stop quickly
          :force-generators
          [(drag-force-generator 1)
           (ground-friction-generator #(to-rect *guy*) 4)
           (gravity-force-generator 10)
           guy-extra-forces]
          
          :velocity-generators
          (conj (keyboard-direction-generators 0.2)
                (jump-velocity-generator #(to-rect *guy*) 3))
          })))

(swap! *entities* conj *guy*)

(defn guy-particle []
  @(:particle *guy*))

(def +viewport-spring-constant+ 20)
(def +viewport-drag-coefficient+ 8)
(def +viewport-max-displacement+ 2)

(defrecord Viewport [particle]
  Rectable
  (to-rect [vp]
    (let [[x y] (:position @particle)]
      [x y 16 10])) ;; expressed in tiles

  Tickable
  (tick [vp]
    (reset! particle (integrate-particle @particle))))

(def *viewport*
  (Viewport.
   (atom
    {:mass 1
     :position [0 0]
     :velocity [0 0]
     
     ;; try to keep the player character basically centered
     :force-generators
     [(fn [p] (spring-force (vec-sub (:position (guy-particle))
                                     (vec-sub (rect-center (viewport-rect))
                                              [0 1]))
                            +viewport-max-displacement+
                            +viewport-spring-constant+))
      (drag-force-generator +viewport-drag-coefficient+)]})))
(swap! *entities* conj *viewport*)

(defn viewport-rect []
  (to-rect *viewport*))

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

(defn tick-entities []
  (doseq [entity @*entities*]
    (tick entity)))

(defn draw-entities []
  (let [ctx (context)]
    (doseq [entity @*entities*]
      (draw entity ctx))))

(defn draw-hud []
  (let [[w h] (img-dims *hud-sprite*)
        [sw sh] *world-dims*]
    (.drawImage (context) *hud-sprite*
                0 0 w h
                0 0 sw sh)))

(defn draw-guy-tile-test []
  (let [ctx (context)]
    (doseq [idx (rect->idxs *current-map* (to-rect *guy*))]
      (let [xy (idx->coords *current-map* idx)]
        (filled-rect ctx xy [1 1] (color [255 0 255]))))))

(defn draw-guy-collision-test []
  (doseq [idx (map-collisions *current-map* (to-rect *guy*))]
    (filled-rect (context) (idx->coords *current-map* idx) [1 1] (color [255 0 255]))))

(defn draw-world []
  (clear)
  (draw-map *current-map*)
  (draw-guy-collision-test)
  (draw-entities)
  (draw-hud))

(defn cycle []
  
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

    (draw-world)

    true))

(defn ^:export main []
  ;;(repl/connect "http://localhost:9000/repl")
  
  (dom/setTextContent (content) "")
  (prepare-display)
  (prepare-input)
  ;;(prepare-sound)
  
  (with-prepared-assets
    (fn []
      (with-img "maps/test.png"
        (fn [img]
          (set! *current-map* (load-map img))
          (until-false cycle (/ +ticks-per-ms+)))))))


