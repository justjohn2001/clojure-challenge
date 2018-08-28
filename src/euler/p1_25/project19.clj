(ns euler.p1-25.project19
  (:require [euler.run :refer [run]]))

(def months '(31 28 31 30 31 30 31 31 30 31 30 31
              31 28 31 30 31 30 31 31 30 31 30 31
              31 28 31 30 31 30 31 31 30 31 30 31
              31 29 31 30 31 30 31 31 30 31 30 31))

(defmethod run 19
  [_]
  (loop [year 1901
         month 0
         days-in-month (cycle months)
         c 0
         dow 2]
    (if (>= year 2001)
      c
      (recur (+ year (quot (inc month) 12))
             (mod (inc month) 12)
             (rest days-in-month)
             (+ c (if (zero? (mod dow 7)) 1 0))
             (+ dow (first days-in-month))))))

