(ns day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input-1 (-> "2024/day1-1.txt" io/resource slurp))

(def example "3   4
4   3
2   5
1   3
3   9
3   3")

(defn numbers [input]
  (let [numbers (-> input
                  str/split-lines
                  (->> (map #(str/split % #" "))
                       (map (juxt first last))
                       flatten
                       (map parse-long)
                       (partition 2)))]
    [(->> numbers (map first) sort)
     (->> numbers (map last) sort)]))

(comment
  (let [[list-1 list-2] (numbers input-1)]
    (reduce + (map (fn [a b] (abs (- a b))) list-1 list-2))))
  ;; => 1941353,

;; part 2
(comment
  (let [[l1 l2] (numbers input-1)
        fr (frequencies l2)]
    (->> l1
         (map (fn [el]
                (-> (get fr el 0)
                    (* el))))
         (remove nil?)
         (reduce +))))
  ;; => 31,
