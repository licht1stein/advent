(ns day6
  (:require [hashp.core]
            [clojure.java.io :as io]))

(def s "mjqjpqmgbljsphdztnvjfqwrcgsmlb")


(defn solve
  ([s]
   (solve s 4))
  ([s n]
   (loop [i 0]
     (if (= n (->> s (take i) (take-last n) set count))
       i
       (recur (inc i))))))


(comment
  (solve "bvwbjplbgvbhsrlpgdmjqwftvncz")     ;; => 5
  (solve "nppdvjthqldpwncqszvftbrmjlhg")     ;; => 6
  (solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ;; => 11

  )

(def data (slurp (io/resource "day6.txt")))

(comment
  (solve data) ;; => 1802
  (solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)
  (solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14)

  (solve data 14) ;; => 3551


  (take-last 4 (take 5 s))
  )
