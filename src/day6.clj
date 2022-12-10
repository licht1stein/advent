(ns day6
  (:require [hashp.core]
            [clojure.java.io :as io]))

(defn solve
  [s & {:keys [n] :or {n 4}}]
  (loop [i 0]
    (if (= n (->> s (take i) (take-last n) set count))
      i (recur (inc i)))))


(def data (slurp (io/resource "day6.txt")))

(comment
  (solve data 14))
