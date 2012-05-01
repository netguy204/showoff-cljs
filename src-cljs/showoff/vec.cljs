(ns showoff.vec)

;;; vectors

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn sub [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])

(defn mag [[ax ay]]
  (Math/sqrt (+ (* ax ax) (* ay ay))))

(defn scale [[ax ay] scale]
  [(* ax scale) (* ay scale)])

(defn unit [v]
  (let [mag (mag v)]
    (if (> mag 0)
      (scale v (/ mag))
      [0 0])))

(defn negate [[ax ay]]
  [(- ax) (- ay)])

(defn dot [[ax ay] [bx by]]
  (+ (* ax bx) (* ay by)))
