(ns euler.p1-25.project5
  (:require [euler.lib.factor :as f]
            [euler.run :as r]))

(defn int-pow [n x]
  (apply * (repeat x n)))

(defmethod r/run 5
  [_ & [n]]
  (reduce (fn [sum [a exp]] (* sum (int-pow a exp)))
          1
          (transduce (comp (take n)
                           (map frequencies)
                           (mapcat seq))
                     (completing
                      (fn [acc [k v]]
                        (update acc k (fnil #(max % v) 0))))
                     {}
                     (f/factor-seq))))

(defn project5 [n]
  (reduce (fn [sum [a exp]] (* sum (int-pow a exp)))
          1
          (reduce (fn [new-hash [k v]]
                    (into new-hash {k (max (get new-hash k 0) v)}))
                  {}
                  (mapcat (fn [h] (map identity h)) (map frequencies (take n (f/factor-seq)))))))

