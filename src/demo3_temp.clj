(ns demo3-temp
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def sample "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def priorities
  (merge
   (zipmap (char-range \a \z) (range 1 27))
   (zipmap (char-range \A \Z) (range 27 53))))

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
       (map find-same)
       (map first)
       (map priorities)
       (reduce +)))

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
  (def s sample))
