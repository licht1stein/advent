(ns day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hashp.core]))


(def sample "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")


(defn make-stacks
  ([stacks]
   (make-stacks stacks (sorted-map)))
  ([data stacks]
   (if-let [[index crate] (first data)]
     (recur (rest data)
            (if (stacks index)
              (update stacks index conj crate)
              (assoc stacks index [crate])))
     stacks)))

(defn parse-stacks [s]
  (let [rows (->> s
                  str/split-lines
                  (take-while seq)
                  (drop-last 1)
                  (map #(partition 3 4 " " %))
                  (map #(map str/join %))
                  (map #(map str/trim %))
                  (map #(map-indexed vector %))
                  (map #(map (fn [[i el]]
                               (when-let [val (->> el (re-find #"[A-Z]" ))]
                                 [(inc i) (first (char-array val))])) %))
                  (map #(filter some? %)))]
    (->> rows
         flatten
         (partition 2)
         reverse
         make-stacks)))

(defn parse-moves [s]
  (->> s
       str/split-lines
       (drop-while seq)
       rest
       (map #(re-seq #"\d+" %))
       (map #(map parse-long %))))

(defn make-mover [process-fn]
  (fn [stacks [n from to]]
    (let [crates (->> (stacks from) (take-last n) process-fn)
          new-stacks     (assoc stacks
                                from (drop-last n (stacks from))
                                to (concat (stacks to) crates))]
      new-stacks)))

(defn process-moves [stacks moves move-fn]
  (if-let [this-move (first moves)]
    (recur (move-fn stacks this-move) (rest moves) move-fn)
    stacks))

(defn parse-and-process [s move-fn]
  (let [stacks (parse-stacks s)
        moves (parse-moves s)
        final (process-moves stacks moves move-fn)]
    final))

(defn solve [s move-fn]
  (->> (parse-and-process s move-fn)
       (map #(-> % last last))
       (apply str)))

(defn solve-part-1 [s]
  (solve s (make-mover reverse)))

(defn solve-part-2 [s]
  (solve s (make-mover identity)))

(def data (slurp (io/resource "day5.txt")))

(comment
  (solve-part-1 sample) ;; => "CMZ"
  (solve-part-1 data)   ;; => "HNSNMTLHQ"

  (solve-part-2 sample) ;; => "MCD"
  (solve-part-2 data)   ;; => "RNLFDJMCT"
  )
