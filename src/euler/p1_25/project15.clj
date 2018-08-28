(ns euler.p1-25.project15
  (:require [euler.run :refer [run]]))

(def routes
  (memoize (fn routes*
             [r c]
             (if (or (= r 0) (= c 0))
               1
               (+ (routes (dec r) c)
                  (routes r (dec c)))))))

(defmethod run 15
  [_ & [r c]]
  (routes r c))
