(ns euler.core
  (:gen-class))

(defn running-sum [n]
  (/ (* n (+ n 1)) 2))

(defn make-summer [n]
  (fn [i]
    (* (running-sum (Math/floor (/ i n)))
       n)))

(defn project1 [n]
  (let [threes (make-summer 3)
        fives (make-summer 5)
        fiveteens (make-summer 15)]
    (- (+ (threes n) (fives n)) (fiveteens n))))

(defn project1-with-loop [n]
  (loop [i 1 sum 0]
    (if (< i n)
      (if (or (= 0 (mod i 3)) (= 0 (mod i 5)))
        (recur (inc i) (+ sum i))
        (recur (inc i) sum))
      sum)
    )
  )

(defn -main
  [& args]
  (println "Project 1 - " (project1 (- 1000 1)))
  (println "Project 1 using loop - " (project1-with-loop 1000)))
