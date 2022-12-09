(ns day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [zprint.core :as zp]
            [hashp.core]))

(def s "30373
25512
65332
33549
35390")

(defn parse [s]
  (->> s
       str/split-lines
       (mapv seq)
       (mapv (fn [el] (mapv #(parse-long (str %)) el)))))


(defn edge-visible [l]
  (+ (* 2 (count l))
     (* 2 (- (count (first l)) 2))))

(defn get-tree [coll x y]
  (-> coll
      (nth  y)
      (nth  x)))

(defn get-top [coll x y]
  (when-let [res (->> coll
                      (take y)
                      (map #(nth % x ))
                      seq)]
    (reverse res)))

(defn get-bottom [coll x y]
  (->> coll
       (drop (inc  y))
       (map #(nth % x))
       seq))

(defn get-left [coll x y]
  (when-let [res (->> (nth coll y)
                      (take x)
                      seq)]
    (reverse res)))

(defn get-right [coll x y]
  (->> (nth coll y)
       (drop (inc x))
       seq))

(defn max? [row]
  (if (seq row)
    (apply max row)
    -1))

(defn visible? [coll x y]
  (let [tree (get-tree coll x y)
        left (get-left coll x y)
        right (get-right coll x y)
        top (get-top coll x y)
        bottom (get-bottom coll x y)
        res (mapv #(> tree (max? %)) [top bottom left right])
        outcome (some true? res)]
    {:y y :x x :? outcome :t tree :u (max? top) :d (max? bottom) :l (max? left) :r (when right (max? right))}))


(defn view [tree row]
  (if (nil? row)
    []
    (loop [nums row
           res []]
      (cond (>= (or (last res) -1) tree) res
            (empty? nums) res
            :else  (recur (rest nums) (conj res (first nums)))))))

(defn solve [s]
  (let [coll (parse s)
        res (for [x (range (count (first coll)))
                  y (range (count coll))]
              (visible? coll x y))]
    #_(pprint res)
    (->> res (filter #(true? (:? %)) ) count)
    ))


(defn viewing-score [coll x y]
  (let [tree (get-tree  coll  x  y)
        viewer (partial view tree)
        left (get-left coll x y)
        right (get-right coll x y)
        top (get-top coll x y)
        bottom (get-bottom coll x y)]
    (->> (map viewer [top bottom left right])
         (map count)
         (reduce *))))

(comment
  (viewing-score coll 0 0)
  )

(defn solve-2 [s]
  (let [coll (parse s)
        res (for [x (range (count (first coll)))
                  y (range (count coll))]
              (viewing-score coll x y))]
    (apply max res)))




(def data (slurp (io/resource "day8.txt")))


(comment
  (for [x (range (count (first psample)))
        y (range (count psample))]
    (do
      (print x y "-> ")
      (println (get-tree psample y x))))

  (range (count (first psample)))
  (range (count psample))
  (def psample (parse sample))
  (->>  (parse sample) (map count))

  (def parsed (parse data))

  (solve s) ;; => 21
  (solve data) ;; => 1798

  ;; => {:y 98, :x 98, :? true, :t 1, :u 5, :d -1, :l 5, :r nil}
  (solve sample)

  (solve-2 s)
  (solve-2 data)
  ;; => 259308

  (def coll (parse s))
  (def dcoll (parse data)))
