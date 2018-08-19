(ns euler.p1-25.project13
  (:require [euler.run :refer [run]]))

(defmethod run 13
  [_ & [numbers]]
  (let [sum (apply + numbers)
        strsum (str sum)]
    (subs strsum 0 10)))

