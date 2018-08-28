(ns euler.p1-25.project16
  (:require [euler.run :refer [run]]))

(defn sub-digits
  ([n] (sub-digits n 0))
  ([n sum]
   (if (zero? n)
     sum
     (recur (bigint (/ n 10))
            (+ sum (mod n 10))))))


(defmethod run 16
  [_ & [n]]
  (sub-digits n))
