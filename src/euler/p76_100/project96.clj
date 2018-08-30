(ns euler.p76-100.project96
  (:require [euler.run :refer [run]]
            [clojure.set :as set]
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
  [[0 0] [0 1] [0 2]
   [1 0] [1 1] [1 2]
   [2 0] [2 1] [2 2]])

(defn solve-board
  ([[name board]] (do (println "Solving by 1 - " name)
                      (solve-board board 0 0)))
  ([board x y]
   (if (> y 8)
     (flatten board)
     (if (zero? (get-in board [x y]))
       (let [x' (- x (mod x 3))
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
       (recur board (mod (inc x) 9) (if (= 8 x) (inc y) y))))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn solve-board-2
  ([[name board]] (do (println "Solving by 2 - " name)
                      (solve-board-2 (into [] (flatten board)) 0)))
  ([board n]
   (cond
     (> n 80) board
     (zero? (nth board n)) (let [x (mod n 9)
                                 y (quot n 9)
                                 box-n (- n (mod n 3) (* 9 (mod (quot n 9) 3)))
                                 unused (apply disj
                                               all-values
                                               (mapcat (juxt #(nth board (+ x (* % 9)))
                                                             #(nth board (+ (* y 9) %))
                                                             #(nth board (+ box-n (mod % 3) (* (quot % 3) 9))))
                                                       (range 9)))]
                            (reduce (fn [board' candidate]
                                       (when-let [solved (solve-board-2 (assoc board n candidate) (inc n))]
                                         (reduced solved)))
                                     nil
                                     unused))
     :else (recur board (inc n)))))

(defn solve-board-3
  ([[name board]] (do (println "Solving by 3 -" name)
                      (solve-board-2 (into [] (flatten board)) 0)))
  ([board n]
   (cond
     (> n 80) board
     (zero? (nth board n)) (let [x (mod n 9)
                                 y (quot n 9)
                                 box-n (- n (mod n 3) (* 9 (mod (quot n 9) 3)))
                                 unused (apply disj
                                               all-values
                                               (concat
                                                (take 9 (drop (- n x) board))
                                                (sequence (comp (drop x)
                                                                (partition-all 9)
                                                                (map first))
                                                          board)
                                                (mapcat #(take 3 %)) (partition-all 9 (drop box-n board))))
                                            ]
                             (reduce (fn [board' candidate]
                                       (when-let [solved (solve-board-2 (assoc board n candidate) (inc n))]
                                         (reduced solved)))
                                     nil
                                     unused))
     :else (recur board (inc n)))))

(defmethod run 96
  [_ file-name]
  (let [boards (read-boards file-name)]
    (transduce (comp (map solve-board-3)
                     (map #(+ (* 100 (first %)) (* 10 (second %)) (nth % 2))))
               +
               0
               boards)))

