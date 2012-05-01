(ns showoff.map
  (:require [showoff.gfx :as gfx]
            [showoff.vec :as vec]
            [showoff.rect :as rect]
            [goog.string.format :as sformat]))

(def format goog.string.format)

(defn idx->coords [map idx]
  (let [[mw _] (:dims map)]
    [(mod idx mw)
     (Math/floor (/ idx mw))]))

(defn coords->idx [map [x y]]
  (let [[mw _] (:dims map)]
    (+ (Math/floor x) (* (Math/floor y) mw))))


(def *alerted-symbols* (atom #{}))

(defn load [img map-symbols]
  (let [[w h] (gfx/img-dims img)
        dims [w h]
        pdata (gfx/get-pixel-data img)
        data (for [idx (range (* w h))]
               (let [pix (gfx/get-pixel-idx pdata (* 4 idx))
                     sym (map-symbols pix)]
                 (if sym
                   (conj sym {:objects (atom #{})
                              :coords (idx->coords {:dims dims} idx)})
                   (when (not (@*alerted-symbols* pix))
                     (swap! *alerted-symbols* conj pix)
                     (js/alert
                      (format "couldn't find map symbol %s" (pr-str pix)))))))]
    
   {:dims dims
    :data (apply array data)}))

(defn draw [ctx map viewport-rect tile-dims]
  (let [[mw mh] (:dims map)
        [ox oy ow oh] viewport-rect
        [tw th] tile-dims
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
          (.drawImage ctx
                      ((aget map-data mapidx) :image)
                      (Math/floor drawx)
                      (Math/floor drawy)))))))

(defn get-map-idx [map idx]
  (let [data (:data map)]
    (aget data idx)))

(defn get-map-coords [map coords]
  (get-map-idx map (coords->idx map coords)))

(defn set-map-idx [map idx newvalue]
  (aset (:data map) idx
        (merge newvalue {:objects (atom #{})
                         :coords (idx->coords map idx)})))

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

(defn with-objects-in-rect [map rect callback]
  (doseq [idx (rect->idxs map rect)]
    (let [rec (get-map-idx map idx)
          objects (:objects rec)]
      (doseq [obj @objects]
        (callback obj)))))

(defn idx->rect [map idx]
  (let [[mw _] (:dims map)]
    [(mod idx mw)
     (Math/floor (/ idx mw))
     1
     1]))

(defn kind-towards? [map rect dir kind]
  (let [center (rect/center rect)
        offset (vec/dir-offset dir)
        test-point (vec/add center offset)
        idx (coords->idx map test-point)
        rec (get-map-idx map idx)]
    (if (kind rec) idx
        
      ;; now we search by offsetting the corners of our rect to make
      ;; sure that we're exhauting everything visually in the
      ;; direction of interest
      (let [ortho-offset (vec/scale (vec/orthogonal-offset dir)
                                    (* 0.5 (rect/edge-length rect dir)))
            test-point2 (vec/add test-point ortho-offset)
            idx2 (coords->idx map test-point2)
            rec2 (get-map-idx map idx2)]
        (if (kind rec2) idx2
            ;; try the other direction
            (let [test-point3 (vec/sub test-point ortho-offset)
                  idx3 (coords->idx map test-point3)
                  rec3 (get-map-idx map idx3)]
              (if (kind rec3) idx3)))))))
