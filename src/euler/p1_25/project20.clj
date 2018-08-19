(ns euler.p1-25.project20
  (:require [euler.run :refer [run]]))

(defmethod run 20
  [_ & [n]]
  (loop [v (apply * 1N (range 2 (inc n)))
         s 0]
    (if (zero? v)
      s
      (recur (quot v 10) (+ s (mod v 10))))))

