(ns showoff.core
  (:use (showoff.showoff :only [get-img remove-entity add-entity supported-by-map
                                Rectable Tickable Drawable
                                vec-add vec-dot vec-scale vec-sub vec-unit
                                rect-center viewport-rect clear display
                                tick-entities tick to-rect draw
                                drag-force-generator gravity-force-generator
                                integrate-particle spring-force
                                apply-particle-vs-map load-map draw-map
                                draw-sprite make-canvas get-img with-img
                                draw-entities img-dims context filled-rect
                                color map-collisions rect->idxs idx->coords
                                resize-nearest-neighbor record-vs-rect
                                set-display-and-viewport cycle-once]))
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

(def *guy-sprites* nil)
(def *hud-sprite* (get-img "hud/hud.png"))

;; a velocity generator function produces a function that takes a
;; particle and returns a velocity
(defn keyboard-velocity-generator [keycode value]
  (fn [p]
    (if (*command-state-map* keycode)
      value
      [0 0])))

(defn jump-velocity-generator [rect-gen up-vel jump-fuel]
  (let [state (atom {:jump-fuel jump-fuel
                     :jumping false})]
   (fn [p]
     (let [supported (supported-by-map *current-map* (rect-gen))]
       (cond
        ;; not trying to jump
        (not (*command-state-map* (.-UP gevents/KeyCodes)))
        (do
          ;; recharge if we're supported
          (when supported
            (swap! state assoc :jump-fuel jump-fuel))
          ;; reset jumping
          (swap! state assoc :jumping false)
          [0 0])

        ;; trying to jump and surface underfoot. give initial
        ;; velocity, no fuel cost
        (and supported (not (:jumping @state)))
        (do
          (swap! state assoc :jumping true)
          [0 (- up-vel)])

        ;; can't jump, we're not supported. try to use fuel
        (:jumping @state)
        (do
          (let [fuel (:jump-fuel @state)]
            (reset! state (conj @state {:jump-fuel (dec fuel)}))

            (if (> fuel 0)
              [0 (- (* (/ fuel jump-fuel) up-vel))]
              [0 0])))

        ;; not supported, not trying to jump
        :else
        [0 0])))))

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
        (vec-scale [1 0] (* -1 coefficient xvel))))))


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

(def +firing-cooldown+ 10)

(defrecord Guy [particle]
  showoff.showoff.Rectable
  (to-rect [guy]
    (let [[x y] (:position @particle)]
      [(+ x 0.2) (+ y 0.1) 0.6 0.9]))

  showoff.showoff.Tickable
  (tick [guy]
    (if (*command-state-map* 32)
      (do
        ;; firing?
        (let [cooldown (or (:firing-cooldown @particle) 0)
              [vx _] (:velocity @particle)
              bullet-direction (if (> vx 0) 1 -1)
              bullet {:mass 1
                      :position (vec-add [(if (> vx 0) 1 0) 0.5] (:position @particle))
                      :velocity [(* bullet-direction 6) 0]}]
          
          (if (> cooldown 0)
            ;; can't fire yet
            (swap! particle conj {:firing-cooldown (- cooldown 1)})
            (do
              ;; fire and reset the counter
              (add-entity *current-map* (Bullet. (atom bullet) 40))
              (swap! *guy-extra-forces* conj [(* -3.0 bullet-direction) 0])
              (swap! particle conj {:firing-cooldown +firing-cooldown+})))))
      ;; not firing
      (swap! particle conj {:firing-cooldown 0}))
    
    (reset! particle (apply-particle-vs-map (integrate-particle @particle)
                                            *current-map*
                                            (to-rect guy)
                                            0.0)))

  showoff.showoff.Drawable
  (draw [guy ctx]
    (let [[vx _] (:velocity @particle)
          sprite (if (> vx 0) (nth *guy-sprites* 0) (nth *guy-sprites* 1))]
      (draw-sprite ctx sprite (:position @particle)))))

(def +guy-speed+ 30)

(def *guy*
  (Guy.
   (atom {:mass 3
          :position [2 2]
          :velocity [0 0]
          
          ;; bring to a stop quickly
          :force-generators
          
          [(drag-force-generator 2.5)
           (ground-friction-generator #(to-rect *guy*) 30)
           (gravity-force-generator 16)
           guy-extra-forces
           (keyboard-velocity-generator
            (.-LEFT gevents/KeyCodes) [(- +guy-speed+) 0])
           (keyboard-velocity-generator
            (.-RIGHT gevents/KeyCodes) [+guy-speed+ 0])
           (jump-velocity-generator #(to-rect *guy*) 300 14)
           ]
          
          })))

(defn guy-particle []
  @(:particle *guy*))

(def +viewport-spring-constant+ 50)
(def +viewport-drag-coefficient+ 3)
(def +viewport-max-displacement+ 2)

(defrecord Viewport [dims particle]
  showoff.showoff.Rectable
  (to-rect [vp]
    (let [[w h] dims
          [x y] (:position @particle)]
      [x y w h])) ;; expressed in tiles

  showoff.showoff.Tickable
  (tick [vp]
    (reset! particle (integrate-particle @particle))))

(def *viewport*
  (Viewport.
   [16 10]
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

(defn with-prepared-assets [callback & {:keys [force-nocache]}]
  ;; a few assets we can resize lazily
  (with-img "hud/hud.png"
    (fn [hud]
      (set! *hud-sprite* (resize-nearest-neighbor hud showoff.showoff.*world-dims*))))

  ;; its critical that +map-symbols+ be built before callback is
  ;; invoked
  (with-img (if force-nocache
              (str "sprites/sheet.png?" (Math/random))
              "sprites/sheet.png")
    (fn [sheet]
      (let [dest-dims showoff.showoff.*tile-in-world-dims*]
        (set! *guy-sprites* [(resize-nearest-neighbor sheet [16 0 16 16] dest-dims)
                             (resize-nearest-neighbor sheet [32 0 16 16] dest-dims)])
        (set! +map-symbols+
              {[255 255 255]
               {:kind :skip}
               
               [255 0 0]
               {:kind :image
                :image (resize-nearest-neighbor sheet [0 16 16 16] dest-dims)
                :collidable true
                :shape :rect}

               ;; slopes down going right
               [255 255 0]
               {:kind :image
                :image (resize-nearest-neighbor sheet [16 16 16 16] dest-dims)
                :collidable true
                :shape :right-triangle
                :slope 1
                :intercept 0
                :top-filled false}

               [153 153 0]
               {:kind :image
                :image (resize-nearest-neighbor sheet [16 32 16 16] dest-dims)
                :collidable true
                :shape :right-triangle
                :slope 1
                :intercept 0
                :top-filled false}


               ;; slopes up going right
               [0 255 0]
               {:kind :image
                :image (resize-nearest-neighbor sheet [32 16 16 16] dest-dims)
                :collidable true
                :shape :right-triangle
                :slope -1
                :intercept 1
                :top-filled false}

               [0 102 51]
               {:kind :image
                :image (resize-nearest-neighbor sheet [32 32 16 16] dest-dims)
                :collidable true
                :shape :right-triangle
                :slope -1
                :intercept 1
                :top-filled false}

               [153 0 0]
               {:kind :image
                :image (resize-nearest-neighbor sheet [0 32 16 16] dest-dims)
                :collidable true
                :shape :rect}
               
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

(def request-animation (or window/requestAnimationFrame
                           window/webkitRequestAnimationFrame
                           window/mozRequestAnimationFrame
                           window/oRequestAnimationFrame
                           window/msRequestAnimationFrame))

(defn until-false [callback timeout]
  (let [step (fn [] (when (callback) (until-false callback timeout)))]
    (timer/callOnce step timeout)
    (comment
      (if (> timeout 0)
       (timer/callOnce step timeout)
       (request-animation step (display))))))

(defn ^:export main []
  ;;(repl/connect "http://localhost:9000/repl")
  
  (dom/setTextContent (content) "")
  
  ;; prepare the display
  (let [screen-dims [640 480]
        canvas (make-canvas screen-dims)
        viewport-fn (fn [] (to-rect *viewport*))]
    (set-display-and-viewport canvas screen-dims viewport-fn)
    (dom/appendChild (content) canvas))

  (prepare-input)
  ;;(prepare-sound)
  
  (with-prepared-assets
    (fn []
      (with-img "maps/test.gif"
        (fn [img]
          (set! *current-map* (load-map img +map-symbols+))
          (add-entity *current-map* *guy*)
          (add-entity *current-map* *viewport*)

          (until-false #(cycle-once draw-world) 0))))))



(defn ^:export map-viewer []
  ;; use a much larger display
  (let [screen-dims [1024 768]
        canvas (make-canvas screen-dims)
        viewport-speed 0.5
        viewport (Viewport.
                  [64 48]
                  (atom
                   {:mass 1
                    :position [0 0]
                    :velocity [0 0]

                    :offset-generators
                    [(keyboard-velocity-generator
                      (.-LEFT gevents/KeyCodes) [(- viewport-speed) 0])
                     (keyboard-velocity-generator
                      (.-RIGHT gevents/KeyCodes) [viewport-speed 0])
                     (keyboard-velocity-generator
                      (.-UP gevents/KeyCodes) [0 (- viewport-speed)])
                     (keyboard-velocity-generator
                      (.-DOWN gevents/KeyCodes) [0 viewport-speed])]}))]
    (dom/appendChild (content) canvas)
    (set-display-and-viewport canvas screen-dims #(to-rect viewport))
    (prepare-input)

    (with-prepared-assets
      (fn []
        (with-img "maps/test.gif"
          (fn [img]
            (set! *current-map* (load-map img +map-symbols+))
            (add-entity *current-map* viewport)
            (until-false #(cycle-once (fn []
                                   (clear)
                                   (draw-map *current-map*)))
                         300)))))

    (gevents/listen
     (by-id "reload") "click"
     (fn []
       (with-prepared-assets
         (fn []
           (with-img (str "maps/test.gif?" (Math/random))
             (fn [img]
               (set! *current-map* (load-map img +map-symbols+)))))
         :force-nocache true)))))