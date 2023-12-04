(ns advent-2023.day04-test
  (:require
   [advent-2023.day04 :as sut]
   [clojure.test :refer [testing]]
   [expectations.clojure.test :refer [defexpect expect]]))

(defexpect parse-card
  (testing "Parse one line of input"
    (expect {:id 1
             :won-ids [2 3 4 5]
             :points 8}
            (sut/parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))))


