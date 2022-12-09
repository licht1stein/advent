(ns day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [zprint.core :as zp]
            [hashp.core]))

(defn parse-command [command]
  (let [cleaned (str/replace command "$ " "")]
    (cond (= cleaned "ls") {:command :ls}
          (= cleaned "cd /") {:command :top}
          (= cleaned "cd ..") {:command :up}
          :else {:command :cd :target (str/replace cleaned "cd " "")})))

(defn parse-file [line]
  (if (str/starts-with? line "dir")
    {:dir (str/replace line "dir " "")}
    (let [[size name] (str/split line #" ")]
      {:file name
       :size (parse-long size)})))

(defn parse-line [line]
  (when line
    (if (str/starts-with? line "$")
      (parse-command line)
      (parse-file line))))

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

(comment
  (parse-line "5626152 d.ext"))

(defn update-sizes [sizes-map path size]
  (loop [path path
         smap sizes-map]
    (if (empty? path)
      smap
      (recur (drop-last path) (if (smap path)
                                (update smap path + size)
                                (assoc smap path size))))))


(defn parse-file-structure [s]
  (loop [line (first (str/split-lines s))
         string (rest (str/split-lines s))
         current-path nil
         file-structure {"/" {}}
         sizes {}]
    (if-let [parsed (parse-line line)]
      (cond
        (nil? line) file-structure
        (= (:command parsed) :top) (recur (first string) (rest string) ["/"] file-structure sizes)
        (= (:command parsed) :ls) (recur (first string) (rest string) current-path file-structure sizes)
        (= (:command parsed) :up) (recur (first string) (rest string) #p (-> current-path drop-last vec) file-structure sizes)
        (= (:command parsed) :cd) (recur (first string) (rest string) (conj current-path (:target parsed)) file-structure sizes)
        (:dir parsed) (recur (first string) (rest string) current-path (if (get-in file-structure (conj current-path (:dir parsed)))
                                                                         file-structure
                                                                         (assoc-in file-structure (conj  current-path (:dir parsed)) {})) sizes)
        (:file parsed) (recur (first string) (rest string) current-path (assoc-in file-structure (conj current-path (:file parsed)) (:size parsed)) (update-sizes sizes current-path (:size parsed))) )

      sizes)))


(defn solve-part-1! [s]
    (->> s
         parse-file-structure
         vals
         (filter #(< % 100000))
         (reduce +)))

(comment
  (def files (parse-file-structure s))

  (def sizes (parse-file-structure s))




  {"/"
   {"a" {"e" {"i" 584}, "f" 29116, "g" 2557, "h.lst" 62596},
    "b.txt" 14848514,
    "c.dat" 8504156,
    "d" {"j" 4060174, "d.log" 8033020, "d.ext" 5626152, "k" 7214296}}})

(defn dir-size
  ([files]
   (dir-size files ["/"]))
  ([files path]
   (let [children (get-in files path)]
     (->>
      (for [[k v] children]
        (if (int? v)
          v
          (dir-size files (conj path k))))
      flatten
      (reduce +)))))

(defn all-dir-children
  ([files]
   (all-dir-children files ["/"]))
  ([files path]
   (->>
    (for [[k v] (get-in files path)
          :when (map? v)]
      (conj path k))
    seq)))

(defn all-directories
  ([files]
   (all-directories files ["/"]))
  ([files path]
   (->>
    (for [dir (all-dir-children files path)]
      (if (all-dir-children files dir)
        [dir (all-directories files dir)]
        [dir]))
    flatten
    (partition-by #(= "/" %))
    (partition 2)
    (map flatten)
    (map vec))))


(defn solve-part-1 [s]
  (let [files (parse-file-structure s)
        dirs (all-directories files)
        sizes (map (partial dir-size files) dirs)]
    (->> sizes
         (filter #(< % 100000))
         (reduce +))))

(defn solve-part-2 [s total-space required-space]
  (let [files (parse-file-structure s)
        dirs (all-directories files)
        sizes (map (partial dir-size files) dirs)
        total-used (dir-size files)
        free-space (- total-space total-used)
        min-del-size (- required-space free-space)]
    (->> sizes
         sort
         (drop-while #(< % min-del-size))
         first)))

(def data (slurp (io/resource "day7.txt")))

(comment


  (dir-size files)
  (all-dir-children files)
  ;; => (["/" "a"] ["/" "d"])

  (def dirs (all-directories files))
  dirs
  ;; => (["/" "a"] ["/" "a" "e"] ["/" "d"])

  (map (partial dir-size files) dirs)

  (solve-part-1! s)    ;; => 95437
  (solve-part-1! data) ;; => 1908462

  (solve-part-2 s 70000000 30000000)    ;; => 24933642
  (solve-part-2 data 70000000 30000000) ;; => 3979145

  (parse-file-structure data)
  )
