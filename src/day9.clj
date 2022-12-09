(ns day9
  (:require [hashp.core]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def initial-state
  {:head {:row 0
          :col 0}
   :tail {:row 0
          :col 0}})

(defn state->str [{:keys [head tail]}]
  (let [rsize (apply max (map :row [head tail]))
        csize (apply max (map :col [head tail]))
        grid-size [(inc (max rsize 4)) (inc (max csize 4))]]
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

(defn print-state [state]
  (let [strstate (state->str state)
        width (-> strstate str/split-lines first str/trim count)]
    (println (str/join (repeat width "=")))
    (println strstate)))

(defn print-states [states & header]
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

(defn tail-adjacent? [{:keys [head tail]}]
  (let [{hrow :row hcol :col} head
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
                 "U" {:part :head :row 1}
                 "D" {:part :head :row (- 1)}
                 "R" {:part :head :col 1}
                 "L" {:part :head :col (- 1)}))]
    (when print?
      (println "--------" s "---------")
      (run! println cmds)
      (println "----------------------"))
    cmds ))

(defn tail-diff [{:keys [head tail] :as state}]
  (when-not (tail-adjacent? state)
    (let [{hrow :row hcol :col} head
          {trow :row tcol :col} tail]
      {:row (- hrow trow) :col (- hcol tcol)})))

(defn tail-instructions [state]
  (when-let [instr  (tail-diff state)]
    (let [{:keys [row col]} instr
          rsign (if (neg? row) - identity)
          csign (if (neg? col) - identity)]
      (merge {:part :tail}
             (cond (= row 0) {:col (csign 1)}
                   (= col 0) {:row (rsign 1)}
                   :else {:row (rsign 1) :col (csign 1)})))))

(defn move-tail [state & {:keys [print?]}]
  (when-let [instr (tail-instructions state)]
    (let [after (move state instr)]
      (when print?
        (print-states state after)
        (println instr))
      after)))

(defn parse [s]
  (->> s str/split-lines (map parse-command) flatten))

(defn solve [s & {:keys [print?]}]
  (loop [state initial-state
         commands (parse s)
         tail-positions #{}]
    (if (empty? commands)
      (->> tail-positions (filter some?) count)
      (let [after-head (move state (first commands))
            after-tail (move-tail after-head)
            new-state (or after-tail after-head)]
        (when print?
          (print-states [state after-head new-state] (dissoc (first commands) :part)))
        (recur new-state (rest commands) (conj tail-positions (:tail after-tail))))
      )))

(def data (slurp (io/resource "day9.txt")))

(def s "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(comment
  (solve s :print? true) ;; => 13
  (solve data)
  ;; => 6384
  )
