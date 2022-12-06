(ns day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))


(def sample
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")


(def data-day-1 (slurp (io/resource "day1.txt")))


(defn sort-elves
  [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)
       (map #(map parse-long %))
       (map #(apply + %))
       sort
       reverse))


(defn solve-part-1
  [s]
  (-> s sort-elves first))


(defn solve-part-2
  [s]
  (->> s sort-elves (take 3) (apply +)))


(comment
  (solve-part-1 data-day-1) ;; => 66487
  (solve-part-2 data-day-1) ;; => 197301
  )
