(ns euler.p1-25.project9
  (:require [euler.run :as r]))

(defmethod r/run 9
  [_ & [n]]
  (reduce (fn [product a]
            (let [b (/ (- (/ (* n n) 2)
                          (* n a))
                       (- n a))]
              (if (= b (int (Math/floor b)))
                (* a b (int (Math/sqrt (+ (* a a) (* b b)))))
                product)))
          0
          (range 1 (/ n 3))))

