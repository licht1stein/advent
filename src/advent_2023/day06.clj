(ns advent-2023.day06
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))


(defn parse
  [input]
  (->> input
       str/split-lines
       (map #(re-seq #"\d+" %))
       (map #(map parse-long %))
       (apply interleave)
       (partition 2)))

(defn win?
  [hold-time race-time record]
  (< record (* hold-time (- race-time hold-time))))

(defn winning-times
  [[race-time record]]
  (->> (for [t (range race-time)]
         (win? t race-time record))
       (filter true?)
       count))

(defn solve
  [input]
  (->> input
       parse
       (map winning-times)
       (reduce *)))

(defn parse-2
  [input]
  (->> input
       str/split-lines
       (map #(re-seq #"\d+" %))
       (map #(apply str %))
       (map parse-long)))

(defn solve-2
  [input]
  (-> input
      parse-2
      winning-times))

(def sample
  "Time:      7  15   30
Distance:  9  40  200")

(def input
  "Time:        40     82     91     66
Distance:   277   1338   1349   1063")

(comment
  (solve sample)  ;; => 288
  (solve input)  ;; => 505494
  (solve-2 sample)  ;; => 71503
  (solve-2 input)  ;; => 23632299
  )
