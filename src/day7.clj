(ns day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hashp.core]))

(defn parse-command [command]
  (when command
    (cond (= command "$ ls") {:command :ls}
          (= command "$ cd /") {:command :top}
          (= command "$ cd ..") {:command :up}
          (str/starts-with? command "$ cd") {:command :cd :target (str/replace command  "$ cd " "")}
          (str/starts-with? command "dir") {:command :dir
                                            :dir (str/replace command "dir " "")}
          :else (let [[size nm] (str/split command #" ")] {:command :file
                                                           :file nm
                                                           :size (parse-long size)}))))

(defn update-sizes [sizes-map path size]
  (loop [path path
         smap sizes-map]
    (if (empty? path)
      smap
      (recur (drop-last path) (if (smap path)
                                (update smap path + size)
                                (assoc smap path size))))))

(defn parse-file-structure [s]
  (loop [lines (str/split-lines s)
         current-path nil
         sizes {}]
    (if-let [parsed (parse-command (first lines))]
      (case (:command  parsed)
        :top (recur (rest lines) ["/"]  sizes)
        :up (recur (rest lines) (-> current-path drop-last vec)  sizes)
        :cd (recur (rest lines) (conj current-path (:target parsed)) sizes)
        :file (recur (rest lines) current-path (update-sizes sizes current-path (:size parsed)))
        (recur (rest lines) current-path sizes))
      sizes)))

(defn solve-part-1 [s]
    (->> s
         parse-file-structure
         vals
         (filter #(< % 100000))
         (reduce +)))

(defn solve-part-2 [s total-space required-space]
  (let [sizes (parse-file-structure s)
        free (- total-space (sizes ["/"]))
        to-delete (- required-space free)]
    (->> sizes
         vals
         (filter #(>= % to-delete))
         (apply min))))

(def data (slurp (io/resource "day7.txt")))

(comment
  (solve-part-1 s)    ;; => 95437
  (solve-part-1 data) ;; => 1908462

  (solve-part-2 s 70000000 30000000)    ;; => 24933642
  (solve-part-2 data 70000000 30000000) ;; => 3979145

  (def s "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")
  )
