(ns euler.p1-25.problem24)

(defn range-1
  ([] (range-1 0))
  ([n] (lazy-seq (cons n (range-1 (inc n)))))
  ([n m] (when (n < m) (lazy-seq (cons n (range-1 (inc n)))))))
