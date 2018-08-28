(ns euler.p1-25.project18
  (:require [euler.run :refer [run]]))

(defn max-of-pairs
  ([a] (max-of-pairs a []))
  ([a maxs]
   (if (< (count a) 2)
     maxs
     (recur (rest a)
            (conj maxs (apply max (take 2 a)))))))

(defmethod run 18
  [_ & [triangle]]
  (loop [tri (reverse triangle)
         sums (repeat (inc (count (last triangle))) 0)]
    (if (= 1 (count sums))
      (first sums)
      (recur (rest tri)
             (mapv + (first tri) (max-of-pairs sums))))))

(defn project18
  ([tri] (project18 tri (into [] (repeat (inc (count (first tri))) 0))))
  ([tri sums]
   (letfn [(max-of-pairs
             ([a] (max-of-pairs a []))
             ([a sums] (if (< (count a) 2)
                         sums
                         (recur (rest a) (conj sums (max (first a) (second a)))))))]
     (if (= 1 (count sums)) (first sums)
         (recur (rest tri) (into [] (map + (first tri) (max-of-pairs sums))))))))

