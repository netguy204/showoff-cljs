(ns showoff.core
  (:use (showoff.showoff :only [get-img remove-entity add-entity supported-by-map
                                Rectable Tickable Drawable
                                vec-add vec-dot vec-scale vec-sub
                                rect-center viewport-rect clear
                                tick-entities tick to-rect draw
                                drag-force-generator gravity-force-generator
                                integrate-particle spring-force
                                apply-particle-vs-map load-map draw-map
                                draw-sprite make-canvas get-img with-img
                                draw-entities img-dims context filled-rect
                                color map-collisions rect->idxs idx->coords
                                resize-nearest-neighbor]))
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

(def *command-state-map* #{})
(def *media-player* nil)
(def *current-map* nil)

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
  (let [[w h] showoff.showoff.*world-dims*]
    (set! showoff.showoff.*display* (make-canvas [w h]))
    (dom/appendChild (content) showoff.showoff.*display*)))

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



(def +map-symbols+ nil) ;; with-prepare-assets will fill this in


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

(def *guy-sprite* (get-img "sprites/guy.png"))
(def *hud-sprite* (get-img "hud/hud.png"))

;; a velocity generator function produces a function that takes a
;; particle and returns a velocity
(defn keyboard-velocity-generator [keycode value]
  (fn [p]
    (if (*command-state-map* keycode)
      value
      [0 0])))

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


(defrecord Bullet [particle maxticks]
  showoff.showoff.Rectable
  (to-rect [bullet]
    (let [[x y] (:position @particle)]
      [x y 0.2 0.2]))
  
  showoff.showoff.Tickable
  (tick [bullet]
    (let [ticks (+ 1 (or (:ticks @particle) 0))]
      (cond
        ;; check for timeout
        (> ticks maxticks)
        (remove-entity *current-map* bullet)

        ;; check for map collisions
        (not (empty? (map-collisions *current-map* (to-rect bullet))))
        (remove-entity *current-map* bullet)
        
        ;; otherwise, integrate - TODO: move-object
        :else
        (reset! particle (conj (integrate-particle @particle)
                               {:ticks ticks})))))

  showoff.showoff.Drawable
  (draw [bullet ctx]
    (let [[x y w h] (to-rect bullet)]
      (filled-rect ctx [x y] [w h] (color [128 128 128])))))

(def *guy-extra-forces* (atom []))
(defn guy-extra-forces [p]
  (let [force (reduce vec-add [0 0] @*guy-extra-forces*)]
    (reset! *guy-extra-forces* [])
    force))

(defrecord Guy [particle]
  showoff.showoff.Rectable
  (to-rect [guy]
    (let [[x y] (:position @particle)]
      [(+ x 0.2) (+ y 0.1) 0.6 0.9]))

  showoff.showoff.Tickable
  (tick [guy]
    ;; firing?
    (when (*command-state-map* 32)
      (let [bullet {:mass 1
                    :position (vec-add [1 0.5] (:position @particle))
                    :velocity [6 0]}]
        (add-entity *current-map* (Bullet. (atom bullet) 40)))
      (swap! *guy-extra-forces* conj [-3 0]))
    
    (reset! particle (apply-particle-vs-map (integrate-particle @particle)
                                            *current-map*
                                            (to-rect guy)
                                            0)))

  showoff.showoff.Drawable
  (draw [guy ctx]
    (draw-sprite ctx *guy-sprite* (:position @particle))))

(def +guy-speed+ 3)

(def *guy*
  (Guy.
   (atom {:mass 1
          :position [2 2]
          :velocity [0 0]
          
          ;; bring to a stop quickly
          :force-generators
          
          [(drag-force-generator 1.2)
           (ground-friction-generator #(to-rect *guy*) 8)
           (gravity-force-generator 6)
           guy-extra-forces
           (keyboard-velocity-generator (.-LEFT gevents/KeyCodes) [(- +guy-speed+) 0])
           (keyboard-velocity-generator (.-RIGHT gevents/KeyCodes) [+guy-speed+ 0])
           (jump-velocity-generator #(to-rect *guy*) 110)
           ]
          
          })))

(defn guy-particle []
  @(:particle *guy*))

(def +viewport-spring-constant+ 20)
(def +viewport-drag-coefficient+ 8)
(def +viewport-max-displacement+ 2)

(defrecord Viewport [particle]
  showoff.showoff.Rectable
  (to-rect [vp]
    (let [[x y] (:position @particle)]
      [x y 16 10])) ;; expressed in tiles

  showoff.showoff.Tickable
  (tick [vp]
    (reset! particle (integrate-particle @particle))))

(set! showoff.showoff.*viewport*
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


(defn with-prepared-assets [callback]
  ;; a few assets we can resize lazily
  (with-img "hud/hud.png"
    (fn [hud]
      (set! *hud-sprite* (resize-nearest-neighbor hud showoff.showoff.*world-dims*))))

  ;; its critical that +map-symbols+ be built before callback is
  ;; invoked
  (with-img "sprites/sheet.png"
    (fn [sheet]
      (let [dest-dims showoff.showoff.*tile-in-world-dims*]
        (set! *guy-sprite* (resize-nearest-neighbor sheet [16 0 16 16] dest-dims))
        (set! +map-symbols+
              {[255 255 255]
               {:kind :image
                :image (resize-nearest-neighbor sheet [0 0 16 16] dest-dims)}
               
               [255 0 0]
               {:kind :image
                :image (resize-nearest-neighbor sheet [0 16 16 16] dest-dims)
                :collidable true}
               
               [136 0 0]
               {:kind :image
                :image (resize-nearest-neighbor sheet [0 32 16 16] dest-dims)
                :collidable true}
               
               [0 0 255]
               {:kind :rect
                :color [0 0 255]}}))
      
      (callback))))



(defn draw-hud []
  (let [[w h] (img-dims *hud-sprite*)
        [sw sh] showoff.showoff.*world-dims*]
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
  ;;(draw-guy-collision-test)
  (draw-entities)
  (draw-hud))

(defn ^:export main []
  ;;(repl/connect "http://localhost:9000/repl")
  
  (dom/setTextContent (content) "")
  (prepare-display)
  (prepare-input)
  (prepare-sound)
  
  (with-prepared-assets
    (fn []
      (with-img "maps/test.png"
        (fn [img]
          (set! *current-map* (load-map img +map-symbols+))
          (add-entity *current-map* *guy*)
          (add-entity *current-map* showoff.showoff.*viewport*)

          (until-false #(cycle draw-world) 0))))))


