(ns advent-2023.day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn eng-symbol?
  [char]
  (not (re-matches #"[\d\.]" (str char))))

(defn digit?
  [char]
  (boolean (re-matches #"\d" (str char))))

(def parse-row
  (memoize
   (fn [row row-index]
     (vec
      (for [[i char] (map-indexed vector row)]
        (cond-> {:col i
                 :row row-index
                 :char char}
          (eng-symbol? char) (assoc :symbol? true)
          (digit? char) (assoc :digit? true)
          (= \* char) (assoc :asterix? true)))))))


(defn adjacent
  ([cell]
   (adjacent cell 1000000 1000000))
  ([{:keys [col row]} max-x max-y]
   (let [xs [(dec col) col (inc col)]
         ys [(dec row) row (inc row)]
         all-neighbors (for [x' xs]
                         (for [y' ys
                               :when (and (>= x' 0) (>= y' 0)
                                          (<= x' max-x) (<= y' max-y))]

                           {:col x' :row y'}))]
     (->> all-neighbors
          (reduce concat)
          (sort-by (juxt :row :col))
          vec))))

(def parse-input
  (memoize
   (fn [input]
     (vec
      (flatten
       (for [[i row] (map-indexed vector (str/split-lines input))]
         (parse-row row i)))))))

(defn find-cell
  [{:keys [row col]} cells]
  (->> cells
       (filter #(and (= row (:row %)) (= col (:col %))))
       first))

(defn collect-number
  [{:keys [row col] :as cell} parsed]
  (let [row (->> parsed (filter #(= row (:row %))))
        this-cell (find-cell cell parsed)
        right (->> row (filter #(> (:col %) col)) (take-while :digit?))
        left (->> row (filter #(< (:col %) col)) reverse (take-while :digit?) reverse)]
    (->> [left this-cell right]
         flatten
         vec)))

(defn cells->num
  [cells]
  (->> cells
       (map :char)
       str/join
       parse-long))

(defn adjacent-numbers
  [cell parsed]
  (->> cell
       adjacent
       (map #(find-cell % parsed))
       (filter :digit?)
       (map #(collect-number % parsed))
       set
       (mapv cell->num)))

(defn adjacent-to
  [key input]
  (let [parsed (parse-input input)]
    (->> parsed
         (filter key)
         (map #(adjacent-numbers % parsed)))))

(defn solve
  [input]
  (->> (adjacent-to :symbol? input)
       flatten
       (reduce +)))

(defn solve-2
  [input]
  (->> (adjacent-to :asterix? input)
       (filter #(= 2 (count %)))
       (map #(reduce * %))
       (reduce +)))


(def sample
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def input (-> "2023/day3.txt" io/resource slurp))

(comment
  (solve sample)  ;; => 4361
  (solve input)  ;; => 526404
  (solve-2 sample)  ;; => 467835
  (solve-2 input)  ;; => 84399773
  )
