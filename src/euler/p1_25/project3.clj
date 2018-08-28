(ns euler.p1-25.project3
  (:require [euler.lib.primes :as primes]
            [euler.run :as r]))

(defmethod r/run 3
  [_ & args]
  (letfn [(project3
            ([n] (project3 n primes/primes))
            ([n p] (let [f (first p)]
                     (cond
                       (= f n) n
                       (= 0 (mod n f)) (recur (/ n f) p)
                       :else (recur n (rest p))))))]
    (apply project3 args)))

