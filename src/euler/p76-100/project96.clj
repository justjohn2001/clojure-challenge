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
  (->> file-name
       slurp
       (string/split #"\n")
       (partition-all 10)
       (map parse-board)))

(def box-deltas 
  '([0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]))

(defn solve-board
  ([board] (solve-board board 0 0))
  ([board x y]
   (cond
     (> y 8) board
     (zero? (get-in board [x y])) (do
                                    (let [x' (- x (mod x 3))
                                          y' (- y (mod y 3))]
                                      (loop [test-value 0]
                                        (when (< test-value 9)
                                          (if (contains? (get board x) test-value)
                                            (recur (inc test-value))
                                            (if (contains? (mapv #(get-in board % y)) test-value)
                                              (recur (inc test-value))
                                              (if (contains? (mapv (fn [[dx dy]] (get-in board (+ x' dx) (+ y' dy)))
                                                                   box-deltas)
                                                             test-value)
                                                (recur (inc test-value))
                                                (or (solve-board (assoc-in board [x y] test-value)
                                                                 (mod (inc x) 9)
                                                                 (if (= 8 x) (inc y) y))
                                                    (recur (inc test-value))))))))))
     :else (solve-board board (mod (inc x) 9) (if (= 8 x) (inc y) y)))))

(def test-board
  ["Grid 02"
   "200080300"
   "060070084"
   "030500209"
   "000105408"
   "000000000"
   "402706000"
   "301007040"
   "720040060"
   "004010003"] )

(defmethod run 96
  [file-name]
  (let [boards (read-boards file-name)]))

