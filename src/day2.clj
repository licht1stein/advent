(ns day2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def rules
  {:rock
   {:value 1
    :beats :scissors
    :loses :paper}

   :paper
   {:value 2
    :beats :rock
    :loses :scissors}

   :scissors
   {:value 3
    :beats :paper
    :loses :rock}

   :outcomes
   {:win 6
    :draw 3
    :lose 0}})

(def strategy
  {:A [:rock :rock]
   :B [:paper :paper]
   :C [:scissors :scissors]
   :X [:rock :lose]
   :Y [:paper :draw]
   :Z [:scissors :win]})

(def sample-strategy-data
  "A Y
B X
C Z")

(defn choose-move
  "Finds desired outcome for move. e.g. (find :scissors :win) => :rock"
  [[move outcome]]
  (case outcome
    :win [move (-> rules move :loses)]
    :lose [move (-> rules move :beats)]
    :draw [move move]))

(defn parse-strategy
  [s & {:keys [part] :or {part :part-1}}]
  (let [data (->> s
                  str/split-lines
                  (map #(str/split % #" "))
                  (map #(map keyword %))
                  (map (fn [e] (map #(-> strategy %) e))))]
    (case part
      :part-1 (map #(map first %) data)
      :part-2 (->> (map #(map second %) data)
                   (map choose-move)))))

(defn outcome
  "Outcomes are calculated for me (second player)."
  [[opponent me]]
  (cond (= (-> rules me :beats) opponent) :win
        (= (-> rules opponent :beats) me) :lose
        :else :draw))

(defn score
  [[opponent me]]
  (let [res (outcome [opponent me])]
    (+ (-> rules :outcomes res) (-> rules me :value))))

(defn strategy-outcome
  [s part]
  (->> (parse-strategy s :part part)
       (map score)
       (reduce +)))

(defonce strategy-data (slurp (io/resource "day2.txt")))

(comment
  (parse-strategy sample-strategy-data :part :part-1) ;; => ((:rock :paper) (:paper :rock) (:scissors :scissors))
  (parse-strategy sample-strategy-data :part :part-2) ;; => ([:rock :rock] [:paper :rock] [:scissors :rock])
  (strategy-outcome sample-strategy-data :part-1) ;; => 15
  (strategy-outcome sample-strategy-data :part-2) ;; => 12
  (strategy-outcome strategy-data :part-1) ;; => 9241
  (strategy-outcome strategy-data :part-2) ;; => 14610
  )
