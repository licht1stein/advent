(ns advent-2023.day04
  (:require
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-card
  [s]
  (let [[card win have] (->> (str/split s #"[\:\|]")
                             (map #(re-seq #"\d+" %))
                             (map #(map parse-long %))
                             (map set))
        intersection (set/intersection win have)
        points (int (math/pow 2 (dec (count intersection))))
        won-ids (range (inc (first card)) (+ (first card) (inc (count intersection))))]
    {:id (first card)
     :won-ids won-ids
     :points points}))

(defn parse-input
  [input]
  (let [parsed-1 (->> input str/split-lines (map parse-card))
        max-id (->> parsed-1 (map :id) (reduce max))
        id-filter (fn [ids] (filter #(<= % max-id)) ids)]
    (map #(update % :won-ids id-filter) parsed-1)))

(defn solve
  [input]
  (->> input
       (parse-input)
       (map :points)
       (reduce +)))

(defn get-won-cards
  [card parsed]
  (vec (flatten (for [id (:won-ids card)]
                  (->> parsed (filter #(= id (:id %))))))))

(defn solve-2
  ([input]
   (solve-2 (parse-input input) (parse-input input) []))
  ([parsed cards stack]
   (if (empty? cards)
     (count stack)
     (let [card (first cards)
           take-cards (take-while #(= (:id %) (:id card)) cards)
           rest-cards (drop-while #(= (:id %) (:id card)) cards)
           won-cards (flatten (repeat (count take-cards) (get-won-cards card parsed)))
           new-cards (sort-by :id (concat won-cards rest-cards))]
       (println "Processing ID:" (:id card))
       (recur parsed new-cards (concat stack take-cards))))))

(def sample
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def input (-> "2023/day4.txt" io/resource slurp))

(comment
  (solve-2 sample) ;; => 30
  (solve-2 input) ;; => 23806951
  )
