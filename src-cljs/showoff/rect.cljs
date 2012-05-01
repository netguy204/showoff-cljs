(ns showoff.rect
  (:require [showoff.vec :as vec]))

(defn center [rect]
  (let [[vx vy vw vh] rect]
    (vec/add [vx vy] (vec/scale [vw vh] 0.5))))

(defn minx [[ax _ _ _]]
  ax)

(defn maxx [[ax _ aw _]]
  (+ ax aw))

(defn miny [[_ ay _ _]]
  ay)

(defn maxy [[_ ay _ ah]]
  (+ ay ah))

(defn width [[_ _ w _]]
  w)

(defn height [[_ _ _ h]]
  h)

(defn minmax [[ax ay aw ah]]
  [ax ay (+ ax aw) (+ ay ah)])

(defn intersect [a b]
  (let [[aminx aminy amaxx amaxy] (minmax a)
        [bminx bminy bmaxx bmaxy] (minmax b)]
   (not
    (or (< amaxx bminx)
        (> aminx bmaxx)
        (< amaxy bminy)
        (> aminy bmaxy)))))

(defn offset [[rx ry rw rh] [ox oy]]
  [(+ rx ox) (+ ry oy) rw rh])

(defn- horizontal-overlap [r1 r2]
  "assumes that there is some overlap"
  (let [[r1x _ r1w _] r1
        [r2x _ r2w _] r2]
    (- (min (+ r1x r1w)
            (+ r2x r2w))
       (max r1x r2x))))

(defn- vertical-overlap [r1 r2]
  "assuems that there is some overlap"
  (let [[_ r1y _ r1h] r1
        [_ r2y _ r2h] r2]
    (- (min (+ r1y r1h)
            (+ r2y r2h))
       (max r1y r2y))))

(defn contact [r1 r2]
  "assumes that r1 and r2 intersect, normal will push r1 from r2"
  (let [ho (horizontal-overlap r1 r2)
        vo (vertical-overlap r1 r2)]
    (cond
      ;; horizontal violation
      (> vo ho)
      (if (< (minx r1) (minx r2))
        {:normal [-1 0]
         :incursion ho}
        {:normal [1 0]
         :incursion ho})

      ;; vertical violation
      (> ho vo)
      (if (< (miny r1) (miny r2))
        {:normal [0 -1]
         :incursion vo} 
        {:normal [0 1]
         :incursion vo})

      ;; corner-shot
      :else
      (cond
        ;; left side
        (< (minx r1) (minx r2))
        (if (< (miny r1) (maxy r2))
          ;; top-left corner
          {:normal (vec/unit [-1 -1])
           :incursion (vec/mag [ho vo])} 
          ;; bottom-left corner
          {:normal (vec/unit [-1 1])
           :incursion (vec/mag [ho vo])})

        ;; right side
        :else
        (if (< (miny r1) (maxy r2))
          ;; top-right corner
          {:normal (vec/unit [1 -1])
           :incursion (vec/mag [ho vo])} 
          ;; bottom-right corner
          {:normal (vec/unit [1 1])
           :incursion (vec/mag [ho vo])})))))

(defn edge-length [rect dir]
  (condp dir =
    :left (height rect)
    :right (height rect)
    :above (width rect)
    :below (width rect)))

