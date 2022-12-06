(ns day3
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def priorities
  (merge
   (zipmap (char-range \a \z) (range 1 27))
   (zipmap (char-range \A \Z) (range 27 53))))

(def sample "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn split-half [s]
  (let [half (/ (count s) 2)]
    [(take half s)
     (drop half s)]))

(defn find-same [s]
  (let [[half-1 half-2] (map set (split-half s))]
    (set/intersection half-1 half-2)))

(defn solve-part-1 [s]
  (->> s
       str/split-lines
       (map str/trim)
       (map find-same)
       (map first)
       (map priorities)
       (reduce +)))

(def data (slurp (io/resource "day3.txt")))

(defn solve-part-2 [s]
  (->> s
       str/split-lines
       (map set)
       (partition 3)
       (map #(apply set/intersection %))
       (map first)
       (map priorities)
       (reduce +)))

(comment
  (def s sample)
  (solve-part-1 sample) ;; => 157
  (solve-part-1 data)   ;; => 7872


  (solve-part-2 sample) ;; => 70
  (solve-part-2 data) ;; => 2497
  
  )

;; GPT
