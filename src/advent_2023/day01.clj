(ns advent-2023.day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def numbers {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn parse-value
  [s]
  (str (or (parse-long s)
           (numbers s))))

;; There was an alternative solution with recursively removing firt char, see
;; https://github.com/licht1stein/advent/commit/2d3b4b59f15bc188aba162173730c3dd184ba74e
(defn parse-line
  [s regex]
  (let [pattern-first (re-pattern (format "^.*?(%s)" regex))
        pattern-last (re-pattern (format ".*(%s).*?$" regex))
        f (last (re-find pattern-first s))
        l (last (re-find pattern-last s))]
    (->> [f l]
         (map parse-value)
         (reduce str)
         parse-long)))

(defn solve
  [input regex]
  (->> input
       str/split-lines
       (map #(parse-line % regex))
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
