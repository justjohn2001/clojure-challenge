(ns euler.p101-125.project122
  (:require [clojure.set :as set]
            [euler.run :refer [run]]))

(defn next-power [known-values]
  {:pre [(sorted? known-values)]}
  (let [[n s] (first known-values)]
    (dissoc
     (reduce (fn rf [acc v]
               (let [m (last v)
                     v-count (count v)
                     current (get acc m)
                     current-count (if current
                                     (count (first current))
                                     Integer/MAX_VALUE)]
                 (update acc
                         m
                         #(cond
                            (< v-count current-count) #{v}
                            (= v-count current-count) (conj % v)
                            :else %))))
             known-values
             (apply set/union
                    (map (fn [v]
                           (let [maximum (last v)]
                             (into #{}
                                   (mapv #(conj v (+ maximum %)) v))))
                         s)))
     n)))

(defn powers-seq
  ([] (powers-seq (sorted-map 1 #{[1]})))
  ([known-values]
   (let [[_ powers] (first known-values)]
     (lazy-seq (cons powers
                     (powers-seq (next-power known-values)))))))

(defmethod run 122
  [_ & [n]]
  (reduce (fn [acc v] (+ acc (dec (count (first v)))))
          0
          (take n (powers-seq))))

