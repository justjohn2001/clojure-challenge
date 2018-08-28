(ns euler.p1-25.project4
  (:require [clojure.string :as str]
            [euler.run :as r]))

(defn palindromic? [n]
  (let [a (seq (str n))
        b (reverse a)]
    (= a b)))

(defn has-3-digit-factors?
  [n]
  (let [sqrt-n (Math/floor (Math/sqrt n))]
    (if (> sqrt-n 999)
      false
      (loop [i sqrt-n]
        (cond
          (< i 100) false
          (> (/ n i) 999) false
          (and (= 0.0 (mod n i)) (= (/ n i) (Math/floor (/ n i)))) true
          :else (recur (dec i)))))))

(defmethod r/run 4
  [_ & params]
  (loop [n (* 999 999)]
    (if (and (palindromic? n) (has-3-digit-factors? n))
      n
      (recur (dec n)))))

