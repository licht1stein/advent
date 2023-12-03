(ns day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hashp.core]))

(defn parse-line [line]
  (if (= "noop" line)
    [{:cmd 0
      :cycles 1}]
    [{:cmd 0
      :cycles 1}
     {:cmd (->> (str/replace line "addx " "") parse-long)
      :cycles 1}]))

(defn filter-keys [m]
  (let [ks (sort (keys m))
        filtered (->> ks rest (partition 2) (map last) (into [(first ks)]))]
    (select-keys m filtered)))

(defn calc-answer-1 [m]
  (->> m vals (reduce +)))

(defn lit? [cycle x]
  (let [row (int (/ cycle 40))
        pixel (- cycle (* row 40))]
    {:row row
     :pixel pixel
     :lit? (>= 1 (abs (- pixel x)))} ))

(defn solve [s divisor]
  (loop [instructions (->> s str/split-lines (map parse-line) flatten)
         x 1
         cycle 0
         strengths {}
         crt []]
    (if (empty? instructions)
      {:cycle cycle :x x :strengths (filter-keys strengths)
       :answer-1 (-> strengths filter-keys calc-answer-1)
       :screen (map #(apply lit? %) crt)}
      (let [{:keys [cmd cycles]} (first instructions)
            new-x (+ x cmd)
            new-cycle (+ cycle cycles)]
        (recur (rest instructions)
               new-x
               new-cycle
               (if (zero?  (mod  new-cycle divisor))
                 (assoc strengths new-cycle (* x new-cycle))
                 strengths)
               (conj crt [cycle x]))))))

(defn print-screen [screen]
  (let [output   (->> (for [pixel screen]
                        (if (:lit? pixel) "🐤" "🕷️"))
                      (partition 40)
                      (map #(apply str %))
                      (str/join "\n"))]
    (println output)
    output))

(def data (slurp (io/resource "day10.txt")))

(comment
  (:answer-1 (solve s 20))    ;; => 13140
  (:answer-1 (solve data 20)) ;; => 14060

  (def r (solve data 20))
  (def screen (:screen r))
  (print-screen screen)
  )
