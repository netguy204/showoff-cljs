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

(def dir-offset {:left [-1 0]
                 :right [1 0]
                 :above [0 -1]
                 :below [0 1]})

(def orthogonal-offset {:left [0 1]
                        :right [0 1]
                        :above [1 0]
                        :below [1 0]})


