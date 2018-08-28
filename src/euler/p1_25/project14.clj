(ns euler.p1-25.project14
  (:require [euler.run :refer [run]]))

(def collatz
  (memoize
   (fn [n]
     (cond
       (= n 1) '(1)
       (even? n) (cons n (lazy-seq (collatz (/ n 2))))
       :else (cons n (lazy-seq (collatz (inc (* n 3)))))))))

(def collatz-count
  (memoize
   (fn cc
     [n]
     (cond
       (= n 1) 1
       (even? n) (inc (cc (/ n 2)))
       :else (inc (cc (inc (* n 3))))))))

(defmethod run 14
  [_ & [n]]
  (apply (partial max-key second)
         (map #(vector % (count (collatz %)))
              (range 1 n)))
  #_(loop [m 1 max-len 0 max-val 0]
    (if (>= m n)
      {max-val max-len}
      (let [new-len (count (collatz m))]
        (if (> max-len new-len)
          (recur (inc m) max-len max-val)
          (recur (inc m) new-len m) )))))

