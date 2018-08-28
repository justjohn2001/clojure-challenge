(ns euler.lib.primes)

(defn reinsert [comb n factors]
  (reduce (fn [acc f]
            (assoc acc
                   (+ n f)
                   (conj (get acc (+ n f) [])
                         f)))
          comb
          factors))

(defn primes-seq
  ([] (primes-seq {} 2))
  ([comb n]
   (if-let [factors (get comb n)]
     (recur (reinsert (dissoc comb n) n factors) (inc n))
     (cons n (lazy-seq (primes-seq (assoc comb (* n n) [n]) (inc n)))))))

(def primes (primes-seq))

