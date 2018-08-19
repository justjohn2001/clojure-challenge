(ns euler.p1-25.project12
  (:require [euler.run :refer [run]]
            [euler.lib.factor :refer [lazy-factor]]))

(defn lazy-triangle
  ([] (lazy-triangle 0 1))
  ([sum n]
   (cons (+ sum n)
         (lazy-seq (lazy-triangle (+ sum n)
                                  (inc n))))))

(defmethod run 12
  [_ & [num-factors]]
  (loop [s (lazy-triangle)]
    (let [n (first s)]
      (if (< num-factors
             (apply * (map (comp inc second)
                           (frequencies (lazy-factor n)))))
        n
        (recur (rest s))))))

