(ns day4
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def sample "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn pair->set [s]
  (let [[from to] (->> s
                       (re-seq #"\d+")
                       (map parse-long))]
    (set (range from (inc to)))))

(defn parse-input [s]
  (->> s
       str/split-lines
       (map #(str/split % #"\,"))
       (map #(map pair->set %))))

(defn superset? [pair-1 pair-2]
  (or (set/superset? pair-1 pair-2)
      (set/superset? pair-2 pair-1)))

(defn solve-part-1 [s]
  (->> s
       parse-input
       (map #(apply superset? %))
       (filter true?)
       count))

(defn solve-part-2 [s]
  (->> s
       parse-input
       (map #(apply set/intersection %))
       (filter seq)
       count))

(def data (slurp (io/resource "day4.txt")))

(comment
  (solve-part-1 sample) ;; => 2
  (solve-part-1 data)   ;; => 538

  (solve-part-2 sample) ;; => 4
  (solve-part-2 data) ;; => 792
  )

(defn handler [request]
  (let [path (:uri request)]
    (cond
      (= path "/status")
      {:status 200 :body "OK"}
      :else
      {:status 404 :body "Not found"})))

(def server
  (doto (java.net.ServerSocket. 8080)
    (.setReuseAddress true)
    (fn [server-socket]
      (while true
        (let [client-socket (.accept server-socket)]
          (future (let [input  (io/reader client-socket)
                        output (io/writer client-socket)]
                    (try
                      (let [request (read-string (slurp input))
                            response (handler request)]
                        (spit output (pr-str response))
                        (flush output))
                      (finally
                        (.close input)
                        (.close output)
                        (.close client-socket))))))))))

(comment (server))
