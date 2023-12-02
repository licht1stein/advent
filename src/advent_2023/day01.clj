(ns advent-2023.day01
  (:require
   [clojure.java.io :as io]
   [clojure.math]
   [clojure.string :as str]
   [hashp.core]))

(def numbers {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn parse-value
  [s]
  (or (parse-long s)
      (numbers s)))

(defn coll->int
  [coll]
  (->> [(-> coll first parse-value) (-> coll last parse-value)]
       (apply str)
       parse-long))

(defn parse-line
  ([regex s]
   (parse-line regex s []))
  ([regex s acc]
   (if (empty? s)
     (->> acc (remove nil?) coll->int)
     (recur regex (str/join (rest s)) (conj acc (re-find regex s))))))

(defn solve
  [input regex]
  (->> input
       str/split-lines
       (map #(parse-line regex %))
       (reduce +)))

(def re-1 #"\d")
(def re-2 #"\d|one|two|three|four|five|six|seven|eight|nine")

(def input (-> "2023/day1.txt" io/resource slurp))

(def sample
  "1abc2
Pqr3stu8vwx
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
