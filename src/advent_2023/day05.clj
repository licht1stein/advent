(ns advent-2023.day05
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-range
  [[dest-start src-start len]]
  (let [dest (range dest-start (+ dest-start len))
        source (range src-start (+ src-start len))]
    (zipmap source dest)))

(defn parse-mapping
  [mapping]
  (reduce merge (map parse-range mapping)))

(def s
  "seed-to-soil map:
931304316 1786548802 232453384
3500539319 2322065235 6421609
496396007 147739714 266329192
3169724489 768672891 39526579
3689153715 1361862036 346985
1936948751 3328259881 542896984
3209251068 3154345676 173914205
1163757700 2814318523 24125066
2484210664 1362209021 231487475
3991904247 2133571422 188493813
1187882766 4045525873 83717994
861951350 3084992710 69352966
2715698139 2838443589 43714032
3830303258 4025104215 20421658
768672891 1268583577 93278459
4180398060 2019002186 114569236
3689500700 1593696496 10659519
1271600760 808199470 460384107
166497091 526585653 102729094
3700160219 3894961176 130143039
2966889400 2882157621 202835089
147739714 414068906 18757377
3850724916 4133608796 141179331
2759412171 2328486844 183672918
2479845735 4129243867 4364929
3480360150 4274788127 20179169
402636637 432826283 93759370
3383165273 2717123646 97194877
3506960928 1604356015 182192787
269226185 629314747 133410452
2943085089 3871156865 23804311
1731984867 2512159762 204963884")

(defn between?
  [n from to]
  (and (>= n from) (<= n to)))

(defn n-in-range?
  [n [_ src len]]
  (between? n src (+ src len)))

(defn n->
  "Get n' for one seq of [dst src len]"
  [n [dst src len]]
  (if (n-in-range? n [dst src len])
    (+ dst (- n src))
    n))

(defn n->>
  "Get n' for seq of seqs of [dst src len]"
  [n colls]
  (let [applicable-coll (first (filter #(n-in-range? n %) colls))]
    (if applicable-coll
      (n-> n applicable-coll)
      n)))


(defn parse-map
  [s]
  (*let [[map-name numbers] (str/split s #":")
         _ (println "Parsing" map-name)
         mname (-> map-name (str/split #" ") first keyword)
         numbers (->> numbers (re-seq #"\d+") (map parse-long) (partition 3))]
        numbers))

(defn parse-input
  [s]
  (let [[seeds & maps-str] (str/split s #"\n\n")
        parsed-seeds (->> seeds (re-seq #"\d+") (map parse-long) vec)]
    {:seeds parsed-seeds :maps (vec (map parse-map maps-str))}))


(defn seed->location
  [n maps]
  (if (empty? maps)
    n
    (recur (n->> n (first maps)) (rest maps))))

(defn solve
  [input]
  (let [{:keys [seeds maps]} (parse-input input)
        locations (map #(seed->location % maps) seeds)]
    (reduce min locations)))

(def input (-> "2023/day5.txt" io/resource slurp))


(defn expand-seed-ranges
  [seeds]
  (->> seeds
       (partition 2)
       (map (fn [[src len]] (range src (+ src len))))
       flatten))

(comment
  (def r [3127166940 109160474])
  (range (first r) (+ 109160474 (first r)))
  )

(defn solve-2
  [input]
  (let [{:keys [maps seeds]} (parse-input input)
        updated (assoc parsed :seeds (expand-seed-ranges seeds))
        locations (map #(seed->location % maps) (:seeds updated))]
    (->> locations
         (map str)
         sort
         first)))

(defn common-prefix)

(comment
  (solve sample)  ;; => 35
  (solve-2 sample)  ;; => 46
  (solve input)
  ;; => 218513636
  (solve-2 input)

  (def parsed (parse-input input))
  (def big-seeds (:seeds parsed))
  (count (expand-seed-ranges big-seeds))
  
  ;; => (3127166940 109160474)

  
  (def parsed (parse-input sample))
  (def seeds (:seeds parsed))
  (expand-seed-ranges seeds)
  
  )

(def largest-match [s1 s2])
(= [\1 \2 \3] [\1 \2 \3])

(def sample
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")
