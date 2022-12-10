(ns day9
  (:require [hashp.core]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def initial-state
  {:head {:row 0
          :col 0}
   :tail {:row 0
          :col 0}})

(defn make-state [length]
  (into (sorted-map) (apply merge (for [i (range (inc length))]
                                    {i {:row 0
                                        :col 0}}))))
(defn states->str [state]
  (let [rsize (apply max 6 (map #(-> (state %) :row) (keys state)))
        csize (apply max 6 (map #(-> (state %) :col) (keys state)))]
    (->>
     (for [row (range rsize)]
       (for [col (range csize)]
         (let [matches (filter #(= (last %) {:row row :col col}) state)]
           matches))))))

#_(defn state->str [{:keys [head tail]}]
    (let [rsize (apply max (map :row [head tail]))
          csize (apply max (map :col [head tail]))
          grid-size [(inc (max rsize 5)) (inc (max csize 5))]]
      (->>
       (for [row (range (first grid-size))]
         (for [col (range (last grid-size))]
           (cond (= {:row row :col col} head) "H"
                 (= {:row row :col col} tail) "T"
                 (and (= 0 row) (= 0 col)) "s"
                 :else ".")))
       reverse
       (map #(str/join " " %))
       (str/join "\n"))))

#_(defn print-state [state]
  (let [strstate (state->str state)
        width (-> strstate str/split-lines first str/trim count)]
    (println (str/join (repeat width "=")))
    (println strstate)))

#_(defn print-states [states & header]
  (let [sstates (map #(->> % state->str str/split-lines) states)
        sep "   "
        width (reduce + (* (count states) (count sep)) (map #(-> % first count) sstates))
        header (if-not (first header) (str/join (repeat width "="))
                       (let [half-w (int (/ (- width (count header)) (count states)))
                             pad (str/join (repeat half-w "="))]
                         (str pad " " header " " pad)))]

    (println "\n" header)
    (->> (apply interleave sstates)
         (partition (count states))
         (map #(str/join "    " %))
         (str/join "\n")
         println)))

(defn move [state {:keys [row col part] :or {row 0 col 0}}]
  (-> state
      (update-in [part :row] + row)
      (update-in [part :col] + col)))

(defn tail-adjacent? [state part]
  (let [tail (state part)
        head (state (- part 1))
        {hrow :row hcol :col} head
        {trow :row tcol :col} tail]
    (or
     (= head tail)
     (and (>= 1 (abs (- hrow trow)))
          (>= 1 (abs (- hcol tcol)))))))

(defn parse-command [s & {:keys [print?]}]
  (let [[direction nstr] (str/split s #" ")
        n (parse-long nstr)
        cmds (for [_ (range n)]
               (case direction
                 "U" {:part 0 :row 1}
                 "D" {:part 0 :row (- 1)}
                 "R" {:part 0 :col 1}
                 "L" {:part 0 :col (- 1)}))]
    (when print?
      (println "--------" s "---------")
      (run! println cmds)
      (println "----------------------"))
    cmds ))

(defn tail-diff [state tail-part]
  (when-not (tail-adjacent? state tail-part)
    (let [tail (state tail-part)
          head (state (- tail-part 1))
          {hrow :row hcol :col} head
          {trow :row tcol :col} tail]
      {:row (- hrow trow) :col (- hcol tcol)})))

(defn tail-instructions [state tail-part]
  (when-let [instr  (tail-diff state tail-part)]
    (let [{:keys [row col]} instr
          rsign (if (neg? row) - identity)
          csign (if (neg? col) - identity)]
      (merge {:part tail-part}
             (cond (= row 0) {:col (csign 1)}
                   (= col 0) {:row (rsign 1)}
                   :else {:row (rsign 1) :col (csign 1)})))
))

(defn parse [s]
  (->> s str/split-lines (map parse-command) flatten))

(defn move-tail [state & {:keys [part print?]}]
  (if-let [instr (tail-instructions state part)]
    (let [after (move state instr)]
      after)
    state))

(defn solve [s & {:keys [print? length]}]
  (loop [state (make-state length)
         commands (parse s)
         tail-positions #{}]
    (if (empty? commands)
      (->> tail-positions (filter some?) count)
      (let [after-head (move state (assoc (first commands) :part 0))
            after-tails (loop [tails (->> state keys sort rest)
                               inner-state after-head]
                          (if (empty? tails)
                            inner-state
                            (recur (rest tails) (move-tail inner-state {:part (first tails)}))))]
        (recur after-tails (rest commands) (conj tail-positions (after-tails (-> state keys sort last))))))))

(def data (slurp (io/resource "day9.txt")))

(comment
  (solve data :length 1) ;; => 6384
  (solve data :length 9) ;; => 2734
  )
