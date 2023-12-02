(ns advent-2023.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-pair
  [s]
  (let [[number color] (re-seq #"\d+|red|green|blue" s)]
    {(keyword color) (parse-long number)}))

(defn parse-set
  [s]
  (merge {:blue 0 :green 0 :red 0}
         (->> (str/split s #",")
              (map parse-pair)
              (reduce merge))))

(defn parse-game
  [s]
  (let [[game-meta game-data] (str/split s #":")
        game-id (->> game-meta (re-find #"\d+") parse-long)
        sets (->> (str/split game-data #";") (mapv parse-set))]
    {game-id sets}))

(defn parse-input
  [s]
  (->> s
       str/split-lines
       (map parse-game)
       (reduce merge)))

(defn set-possible?
  [this-set condition]
  (->> (merge-with > this-set condition)
       (map last)
       (every? false?)))

(defn game-possible?
  [[id sets] condition]
  {id (->> sets
           (map #(set-possible? % condition))
           (every? true?))})

(defn solve
  [input condition]
  (let [games (parse-input input)
        rule (parse-set condition)
        outcomes (map #(game-possible? % rule) games)]
    (->> outcomes
         (filter #(last (last %)))
         (map keys)
         flatten
         (reduce +))))

(defn game-power
  [game]
  (->> game
       (apply merge-with max)
       vals
       (filter #(not= % 0))
       (reduce *)))

(defn solve-2
  [input]
  (->> input
       parse-input
       vals
       (map game-power)
       (reduce +)))

(def condition "Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes.")
(def input (-> "2023/day2.txt" io/resource slurp))

(comment
  (solve input condition)  ;; => 2278
  (solve-2 input)  ;; => 67953
  )

