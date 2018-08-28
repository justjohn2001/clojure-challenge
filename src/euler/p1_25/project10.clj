(ns euler.p1-25.project10
  (:require [euler.run :as r]
            [euler.lib.primes :refer [primes]]))

(defmethod r/run 10
  [_ & [n]]
  (reduce + (take-while #(< % n) primes)))

