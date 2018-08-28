(ns euler.p1-25.project6
  (:require [euler.run :as r]))

(defn running-sum [n]
  (/ (* n (+ n 1)) 2))

(defmethod r/run 6
  [_ & [n]]
  (- (+ (* (running-sum n) (running-sum n)))
     (reduce + (map #(* % %) (range 1 (inc n)))))
  )

