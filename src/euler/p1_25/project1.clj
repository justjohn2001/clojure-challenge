(ns euler.p1-25.project1
  (:require [euler.run :as r]))

(defn threes-and-fives
  ([] (threes-and-fives 1))
  ([i] (if (or (zero? (mod i 3))
               (zero? (mod i 5)))
         (cons i (lazy-seq (threes-and-fives (inc i))))
         (recur (inc i)))))

(defmethod r/run 1
  [_ & [n]]
  (reduce + (take-while #(< % n) (threes-and-fives))))

