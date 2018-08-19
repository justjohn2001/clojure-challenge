(ns euler.lib.factor
  (:require [euler.lib.primes :as primes]))

(defn lazy-factor
  ([n] (lazy-factor n primes/primes))
  ([n p] (let [f (first p)]
           (cond
             (= n 1) nil
             (zero? (mod n f)) (cons f (lazy-seq (lazy-factor (/ n f) p)))
             :else (recur n (rest p))))))

(defn factor-seq
  ([] (factor-seq 1))
  ([n] (cons (lazy-factor n) (lazy-seq (factor-seq (inc n))))))

