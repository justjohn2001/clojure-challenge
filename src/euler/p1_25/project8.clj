(ns euler.p1-25.project8
  (:require [euler.run :as r]))

(defmethod r/run 8
  [_ & [l n]]
  (let [digit-list (map #(- (int %) 48) (seq l))]
    (loop [seqs '() m n]
      (if (zero? m)
        (apply max (apply map * seqs))
        (recur (conj seqs (drop (- n m) digit-list)) (dec m))))))

