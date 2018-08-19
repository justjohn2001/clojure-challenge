(ns euler.p1-25.project11
  [:require [euler.run :refer [run]]])

(defmethod run 11
  [_ & [grid]]
  (loop [grid grid current-max 0]
    (if (< (count grid) 4)
      current-max
      (let [[r1 r2 r3 r4] grid]
        (recur (rest grid)
               (reduce max
                       current-max
                       (concat
                        (map * r1 (drop 1 r1) (drop 2 r1) (drop 3 r1))
                        (map * r1 r2 r3 r4)
                        (map * (drop 3 r1) (drop 2 r2) (drop 1 r3) r4)
                        (map * r1 (drop 1 r2) (drop 2 r3) (drop 3 r4)))))))))

