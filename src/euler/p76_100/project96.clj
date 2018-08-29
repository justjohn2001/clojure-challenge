(ns euler.p76-100.project96
  (:require [euler.run :refer [run]]
            [clojure.string :as string]))

(defn parse-board
  [[name & board]]
  [name (mapv (fn [line]
                (mapv #(Integer/parseInt (str %))
                      (seq line)))
              board)])

(defn read-boards
  [file-name]
  (->> (string/split (slurp file-name) #"\n")
       (partition-all 10)
       (map parse-board)))

(def box-deltas
  '([0 0] [0 1] [0 2]
    [1 0] [1 1] [1 2]
    [2 0] [2 1] [2 2]))

(defn solve-board
  ([[name board]] (solve-board board 0 0))
  ([ board x y]
   (cond
     (> y 8) board
     (zero? (get-in board [x y])) (let [x' (- x (mod x 3))
                                        y' (- y (mod y 3))]
                                    (loop [test-value 0]
                                      (cond
                                        (> test-value 9) nil
                                        (some (partial = test-value) (get board x)) (recur (inc test-value))
                                        (some (partial = test-value) (mapv #(get-in board [% y]) (range 9))) (recur (inc test-value))
                                        (some (partial = test-value) (mapv (fn [[dx dy]] (get-in board [(+ x' dx) (+ y' dy)])) box-deltas)) (recur (inc test-value))
                                        :else (or (solve-board (assoc-in board [x y] test-value)
                                                               (mod (inc x) 9)
                                                               (if (= x 8) (inc y) y))
                                                  (recur (inc test-value))))))
     :else (solve-board board (mod (inc x) 9) (if (= 8 x) (inc y) y)))))

(defmethod run 96
  [_ file-name]
  (let [boards (read-boards file-name)]
    (transduce (comp (map solve-board)
                     (map first)
                     (map #(+ (* 100 (first %)) (* 10 (second %)) (nth % 2))))
               +
               0
               boards)))

