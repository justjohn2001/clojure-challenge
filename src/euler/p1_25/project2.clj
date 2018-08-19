(ns euler.p1-25.project2
  (:require [euler.run :as r]))

(defn fibonacci-seq
  ([] (fibonacci-seq 0 1))
  ([f1 f2] (cons f2 (lazy-seq (fibonacci-seq f2 (+ f1 f2))))))

(defmethod r/run 2
  [_ & [n]]
  (transduce (comp (take-while #(< % n))
                   (filter even?))
             +
             (fibonacci-seq)))

(defn original-run
  [n]
  (reduce + (filter even? (take-while #(< % n) (fibonacci-seq)))))
