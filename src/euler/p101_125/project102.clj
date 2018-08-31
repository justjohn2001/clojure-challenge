(ns euler.p101-125.project102
  (:require [euler.run :refer [run]]
            [clojure.string :as string]))

(defn read-file
  [file-name]
  (->> file-name
      slurp
      string/split-lines
      (mapv #(string/split % #","))
      (mapv (fn [triangle] (mapv #(Integer/parseInt %) triangle)))
      (mapv (partial partition-all 2))))

(defn check-intercepts
  [[x1 y1] [x2 y2] [x3 y3]]
  (let [intercept1-2 (- y1 (* x1 (/ (- y1 y2) (- x1 x2))))
        intercept2-3 (- y3 (* x3 (/ (- y3 y2) (- x3 x2))))]
    (neg? (* intercept1-2 intercept2-3))))

(defn triangle-around-origin?
  [[[x1 y1 :as p1] [x2 y2 :as p2] [x3 y3 :as p3]]]
  (if (neg? (* x1 x2))
    (if (neg? (* x1 x3))             ; p1 and p2 are on oposite sites
      (check-intercepts p2 p1 p3)    ; p1 and p3 are on oposite sides so p2 and p3 are on the same side
      (check-intercepts p1 p2 p3))   ; p1 and p3 are on the same side
    (if (neg? (* x1 x3))             ; p1 and p2 are on the same side
      (check-intercepts p1 p3 p2)     ; p1 and p3 are on oposite sides
      false                          ; all 3 are on the same side
      )))

(defmethod run 102
  [_ file-name]
  (count (filter identity (map triangle-around-origin? (read-file file-name)))))
