(ns euler.p51-75.project75
  (:require [euler.run :refer [run]]))

;; from http://mathworld.wolfram.com/PythagoreanTriple.html

(defn mult-u
  [[a b c]]
  [1 [(+ a (* -2 b) (* 2 c))
      (+ (* 2 a) (- b) (* 2 c))
      (+ (* 2 a) (* -2 b) (* 3 c))]])

(defn mult-a
  [[a b c]]
  [1 [(+ a (* 2 b) (* 2 c))
      (+ (* 2 a) b (* 2 c))
      (+ (* 2 a) (* 2 b) (* 3 c))]])

(defn mult-d
  [[a b c]]
  [1 [(+ (- a) (* 2 b) (* 2 c))
      (+ (* -2 a) b (* 2 c))
      (+ (* -2 a) (* 2 b) (* 3 c))]])

(defn insert
  ([v [n [a b c :as e]]]
   (when (not= (+ (* a a) (* b b)) (* c c))
     (throw (ex-info {:cause e})))
   (let [l (* n (apply + e))
         f (fn [[n e]] (< (* n (apply + e)) l))]
     (vec (concat (take-while f v)
                  [[n e]]
                  (drop-while f v)))))
  ([v e & more]
   (apply insert (insert v e) more)))

(defn len [[n v]] (* n (apply + v)))

(defn pathagorean-triples
  ([] (pathagorean-triples (sorted-map (len [1 [3 4 5]]) [[1 [3 4 5]]])))
  ([sm]
   (let [[k [[n v] & more]] (first sm)
         t (if (seq? more)
             (update sm k rest)
             (dissoc sm k))
         new-t (if (= n 1)
                 (reduce (fn [acc rv]
                           (update acc (len rv)
                                   #(conj (or % []) rv)))
                         t
                         (map #(% v) [mult-u mult-d mult-a]))
                 t)
         new-v [(inc n) v]]
     (cons (vec (map (partial * n) v))
           (lazy-seq (pathagorean-triples (update new-t (len new-v) #(conj (or % []) new-v))))))))

(defmethod run 75
  [_ n]
  (count (sequence (comp (take-while #(<= (apply + %) n))
                         (partition-by (partial apply +))
                         (filter #(= 1 (count %))))
                   (pathagorean-triples))))

