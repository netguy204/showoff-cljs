(ns showoff.gfx
  (:require [goog.dom :as dom]))

(defn get-img [url]
  (let [img (js/Image.)]
    (set! (.-src img) url)
    img))

(defn with-img [url callback]
  (let [img (get-img url)]
    (set! (.-onload img) (fn [] (callback img)))
    img))

(defn make-canvas [[w h]]
  (let [canvas (dom/createDom "canvas")]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    canvas))

(defn context [canvas]
  (let [ctx (.getContext canvas "2d")]
    (set! (.-imageSmoothingEnabled ctx) false)
    ctx))

(defn clear [canvas]
  (let [w (.-width canvas)
        h (.-height canvas)
        ctx (context canvas)]
    (.clearRect ctx 0 0 w h)))

(defn img-dims [img]
  [(.-width img) (.-height img)])

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
    (get-pixel-idx pdata idx)))

(defn get-pixel-alpha [pdata x y]
  (let [[w h] (:dims pdata)
        data (:data pdata)
        idx (+ (* x 4) (* w y 4))]
    (aget data (+ idx 3))))

(defn map-nearest-neighbor [pdata src-rect dest-dims function]
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
        (function dest-data dest-base src-data src-base)))
    (.putImageData ctx dest-image-data 0 0)
    canvas))

(defn pixel-identity [dest-data dest-base src-data src-base]
  (aset dest-data dest-base (aget src-data src-base))
  (aset dest-data (+ 1 dest-base) (aget src-data (+ 1 src-base)))
  (aset dest-data (+ 2 dest-base) (aget src-data (+ 2 src-base)))
  (aset dest-data (+ 3 dest-base) (aget src-data (+ 3 src-base))))

(defn resize-nearest-neighbor
  ([pdata dest-dims]
     (let [[w h] (:dims pdata)]
       (resize-nearest-neighbor pdata [0 0 w h] dest-dims)))
  
  ([pdata src-rect dest-dims]
     (map-nearest-neighbor pdata src-rect dest-dims pixel-identity)))

(defn slice-sprite-image [pdata [x y]]
  (let [[sw sh] showoff.showoff.*tile-dims*]
    (resize-nearest-neighbor pdata [(* x sw) (* y sh) sw sh]
                             showoff.showoff.*tile-in-world-dims*)))

(defn slice-sprite [pdata desc]
  (if-let [img-coords (:image desc)]
    (conj desc {:image (slice-sprite-image pdata img-coords)})
    desc))

(defn slice-sprites [pdata sprite-map]
  (let [sprite-keys (keys sprite-map)
        sprites (map #(slice-sprite pdata (sprite-map %)) sprite-keys)]
    (zipmap sprite-keys sprites)))

