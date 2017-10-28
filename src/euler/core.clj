(ns euler.core
  (:gen-class))

(defn running-sum [n]
  (/ (* n (+ n 1)) 2))

(defn make-summer [n]
  (fn [i]
    (* (running-sum (Math/floor (/ i n)))
       n)))

(defn project1 [n]
  "Sum of numbers less than 1000 that are multiples of 3 or 5"
  (let [threes (make-summer 3)
        fives (make-summer 5)
        fiveteens (make-summer 15)]
    (- (+ (threes n) (fives n)) (fiveteens n))))

(defn project1-with-loop [n]
  (loop [i 1 sum 0]
    (if (>= i n)
      sum
      (if (or (= 0 (mod i 3)) (= 0 (mod i 5)))
        (recur (inc i) (+ sum i))
        (recur (inc i) sum)))))

(defn project2 [n]
  "Sum of even Fibonacci numbers less that 4000000"
  (loop [fib1 1 fib2 2 sum 0]
    (if (>= fib2 n)
      sum
      (recur fib2 (+ fib2 fib1) (if (= 0 (mod fib2 2))
                                  (+ sum fib2)
                                  sum)))))

(defn project3 [n]
  "Larges Prime Factor"
  (loop [partial n]
    (if (is-prime? partial)
      partial
      (recur (/ partial (factor partial))))
    )
  )

(defn primes
  ;I found a more efficient algorithm online, but since I am learning I thought I would write one myself
  ([] (primes (set nil) 2))
  ([found_primes n]
    (if (some #(= 0 (mod n %)) found_primes)
      (recur found_primes (inc n))
      (cons n (lazy-seq (primes (conj found_primes n) (inc n)))))))

(defn project3
  ([n] (project3 n (primes)))
  ([n p] (let [f (first p)]
    (cond
      (= f n) n
      (= 0 (mod n f)) (recur (/ n f) p)
      :else (recur n (rest p))))))

(require '[clojure.string :as str])

(defn palindromic? "test whether a number is palindromic"
  [n]
  (let [a (rest (str/split (str n) #""))]
    (= a (reverse a))))
 
(defn has-3-digit-factors
  [n]
  (let [sqrt-n (Math/floor (Math/sqrt n))]
    (if
      (> sqrt-n 999) false
      (loop [i sqrt-n]
        (cond
          (< i 100) false
          (> (/ n i) 999) false
          (and (= 0.0 (mod n i)) (= (/ n i) (Math/floor (/ n i)))) true
          :else (recur (dec i)))))))

(defn project4 []
  (loop [n (* 999 999)]
    (if (and (palindromic? n) (has-3-digit-factors n))
      n
      (recur (dec n)))))

(defn factor [n]
  (loop [working-n n factors '() p (primes)]
    (let [f (first p)]
      (cond 
        (= f working-n) (conj factors f)
        (= 0 (mod working-n f)) (recur (/ working-n f) (conj factors f) p)
        :else (recur working-n factors (rest p))))))

(defn problem5 []
  (factor 4)
)

(defn project7 [n]
  (last (take n (primes))))

(defn -main
  [& args]
  (println "Project 1 - " (project1 (- 1000 1)))
  (println "Project 1 using loop - " (project1-with-loop 1000))
  (println "Project 2 - " (project2 4000000))
  (println "Project 3 - " (project3 6857))
  (println "Project 4 - " (project4))
  (println "Project 7 - " (project7 10001))
  )
