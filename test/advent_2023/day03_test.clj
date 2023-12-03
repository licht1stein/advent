(ns advent-2023.day03-test
  (:require
   [advent-2023.day03 :as sut]
   [clojure.test :refer [testing]]
   [expectations.clojure.test :refer [defexpect expect]]))

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

(defexpect parse-row
  (testing "Use more explicit data"
    (expect [{:col 0
              :row 0
              :char \4
              :digit? true}
             {:col 1
              :row 0
              :char \6
              :digit? true}
             {:col 2
              :row 0
              :char \7
              :digit? true}
             {:col 3
              :row 0
              :char \.}
             {:col 4
              :row 0
              :char \*
              :symbol? true
              :asterix? true}])
    (sut/parse-row "467.*" 0)))

(defexpect eng-symbol?
  (testing "Determine if a char is a symbol"
    (expect some? (sut/eng-symbol? \!))
    (expect false (sut/eng-symbol? \.))
    (expect some? (sut/eng-symbol? \*))))

(defexpect digit?
  (expect false (sut/digit? \.))
  (expect true (sut/digit? \1)))

(defexpect adjacent
  (testing "Coordinates for all adjacent points"
    (expect [{:col 0 :row 0} {:col 1 :row 0}
             {:col 0 :row 1} {:col 1 :row 1}]
            (sut/adjacent {:col 0 :row 0} 10 10))
    (expect [{:col 9 :row 9} {:col 10 :row 9}
             {:col 9 :row 10} {:col 10 :row 10}] (sut/adjacent {:col 10 :row 10} 10 10))
    (expect [{:col 0 :row 0} {:col 1 :row 0} {:col 2 :row 0}
             {:col 0 :row 1} {:col 1 :row 1} {:col 2 :row 1}
             {:col 0 :row 2} {:col 1 :row 2} {:col 2 :row 2}] (sut/adjacent {:col 1 :row 1} 10 10))))

(defexpect adjacent-numbers
  (testing "Find all adjacent numbers to the cell"
    (expect [35 467] (sut/adjacent-numbers {:row 1 :col 3} (sut/parse-input sample)))))

(defexpect find-cell
  (testing "Get cell from seq of cells by coordinates"
    (expect {:col 0 :row 0}
            (sut/find-cell {:col 0 :row 0} [{:col 0 :row 0}
                                            {:col 1 :row 1}]))))

(defexpect adjacent-digits
  (testing "Find all adjacent digits to each symbol"
    (expect [{:col 2, :row 0, :char \7, :digit? true}
             {:col 2, :row 2, :char \3, :digit? true}
             {:col 3, :row 2, :char \5, :digit? true}
             {:col 6, :row 2, :char \6, :digit? true}
             {:col 7, :row 2, :char \3, :digit? true}
             {:col 2, :row 4, :char \7, :digit? true}
             {:col 4, :row 6, :char \2, :digit? true}
             {:col 2, :row 9, :char \6, :digit? true}
             {:col 3, :row 9, :char \4, :digit? true}
             {:col 6, :row 7, :char \7, :digit? true}
             {:col 5, :row 9, :char \5, :digit? true}
             {:col 6, :row 9, :char \9, :digit? true}]
            (sut/adjacent-digits (sut/parse-input sample)))))


(defexpect collect-number
  (testing "Collect a number from a cell of one of it's digits"
    (expect [{:col 0, :row 0, :char \4, :digit? true}
             {:col 1, :row 0, :char \6, :digit? true}
             {:col 2, :row 0, :char \7, :digit? true}]
            (sut/collect-number {:col 0 :row 0} (sut/parse-input sample)))
    (expect [{:col 5, :row 9, :char \5, :digit? true}
             {:col 6, :row 9, :char \9, :digit? true}
             {:col 7, :row 9, :char \8, :digit? true}]
            (sut/collect-number {:col 6 :row 9} (sut/parse-input sample)))))

(defexpect collect-adjacent-numbers
  (testing "Collect all adjacent numbers from parsed"
    (expect
     #{[{:col 5, :row 9, :char \5, :digit? true}
        {:col 6, :row 9, :char \9, :digit? true}
        {:col 7, :row 9, :char \8, :digit? true}]
       [{:col 2, :row 2, :char \3, :digit? true}
        {:col 3, :row 2, :char \5, :digit? true}]
       [{:col 2, :row 6, :char \5, :digit? true}
        {:col 3, :row 6, :char \9, :digit? true}
        {:col 4, :row 6, :char \2, :digit? true}]
       [{:col 6, :row 2, :char \6, :digit? true}
        {:col 7, :row 2, :char \3, :digit? true}
        {:col 8, :row 2, :char \3, :digit? true}]
       [{:col 0, :row 4, :char \6, :digit? true}
        {:col 1, :row 4, :char \1, :digit? true}
        {:col 2, :row 4, :char \7, :digit? true}]
       [{:col 1, :row 9, :char \6, :digit? true}
        {:col 2, :row 9, :char \6, :digit? true}
        {:col 3, :row 9, :char \4, :digit? true}]
       [{:col 0, :row 0, :char \4, :digit? true}
        {:col 1, :row 0, :char \6, :digit? true}
        {:col 2, :row 0, :char \7, :digit? true}]
       [{:col 6, :row 7, :char \7, :digit? true}
        {:col 7, :row 7, :char \5, :digit? true}
        {:col 8, :row 7, :char \5, :digit? true}]}
     (sut/collect-adjacent-numbers (sut/parse-input sample)))))

