(ns euler.p1-25.project7
  (:require [euler.lib.primes :refer [primes]]
            [euler.run :as r]))

(defmethod r/run 7
  [_ & [n]]
  (last (take n primes)))

