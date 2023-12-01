(ns advent-2023.day01
  (:require
   [clojure.java.io :as io]
   [clojure.math]
   [clojure.string :as str]
   [hashp.core]))

(def numbers {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn pair?
  [s1 s2]
  (when (str/ends-with? s1 (-> s2 first str))
    [(str (str/join (butlast s1)) s2) (str s1 s2)]))

(def replacements
  (->>
   (for [one (keys numbers)]
     (for [two (keys numbers)]
       (or (pair? one two) (pair? two one))))
   flatten
   (remove nil?)
   (partition 2)))

(defn replace-pair
  [s [wrong right]]
  (str/replace s wrong right))

(defn sanitize
  ([s]
   (sanitize-input s replacements))
  ([s pairs]
   (if (empty? pairs)
     s
     (recur (replace-pair s (first pairs)) (rest pairs)))))

(defn parse-value
  [s]
  (or (parse-long s)
      (numbers s)))

(defn solve
  [input regex]
  (->> input
       sanitize
       str/split-lines
       (map #(re-seq regex %))
       (map #(list (first %) (last %)))
       (map #(map parse-value %))
       (map #(reduce str %))
       (map parse-long)
       (reduce +)))

(def re-1 #"\d")
(def re-2 #"\d|one|two|three|four|five|six|seven|eight|nine")

(def input (-> "2023/day1.txt" io/resource slurp))

(def sample
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def sample-2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(comment
  (solve sample re-1)  ;; => 142
  (solve input re-1) ;; => 54597
  (solve sample-2 re-2)  ;; => 281
  (solve input re-2)  ;; => 54504
  )
