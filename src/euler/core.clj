(ns euler.core
  (:gen-class))

(defn running-sum [i]
  (/ (* i (+ i 1)) 2))

(defn make-summer [n]
  (fn [i]
    (* (running-sum (Math/floor (/ i n)))
       n)))

(defn project1 [n]
  (let [threes (make-summer 3)
        fives (make-summer 5)
        fiveteens (make-summer 15)]
    (- (+ (threes n) (fives n)) (fiveteens n))))

(defn -main
  [& args]
  (println "Project 1 - " (project1 (- 1000 1))))
