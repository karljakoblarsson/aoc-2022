(ns aoc.day7
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.core.match :as match]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 7)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-line [l]
  (match/match l
    ["$" "cd" path] { :cmd :cd :arg path }
    ["$" "ls"] { :cmd :ls }
    ["dir" n] { :dir n }
    [size n] { :file n :size (Integer/parseInt size) }
    )
  )

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        words (map #(s/split % #" ") lines)
        ]
  (map parse-line words)))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; t1

(defn walk-history [{:keys [current tree] :as state} line]
  ; (println line)
  ; (println state)
  (match/match line
    { :cmd :cd :arg path } (if (= path "..")
                             (update state :current pop)
                             (update state :current #(conj % path))
                             )
    { :cmd :ls } state
    { :dir n } (update-in state [:tree current] #(conj % n))
    { :file n :size size } (update-in state [:tree current] #(conj % size ))
    )
  )

(defn build-tree [lines]
  (:tree (reduce walk-history { :current [] :tree {} } lines) ))

(def t2 (build-tree t1))
(print t2)

(defn sum-sizes [tree]
  (reduce (fn [m [k v]] (assoc m k (sum (filter int? v))) ) {} tree))

(def t3 (sum-sizes t2))
; t3
(println t3)

(defn starts-with [a b]
  (if (empty? a)
    true
    (if (= (first a) (first b))
      (recur (rest a) (rest b))
      false
      )
    )
  )

(starts-with [1 2 3] [1 2 3])

; (map second (filter (fn [[k v]] (starts-with k ["/" "a"])) t3))

(defn shorter-paths [path]
  (map reverse (reduce (fn [acc el] (conj acc (conj (last acc) el)))  []  path) )
  )

; (shorter-paths ["/" "a" "e"])

(defn totals [tree]
  (let [folders (keys tree) ]
    (map
      (fn [path]
       (let [sizes (map second (filter (fn [[k v]] (starts-with path k)) tree))]
         ; (print path)
         ; (println sizes)
         [path (sum sizes)]
         )
       )
      folders)
    ))

; (println (totals t3))
; (sum (filter #(< % 100000) (map second (totals t3))))

(defn part1 [in]
  (let [tree (-> in build-tree sum-sizes totals)]
    ; (print tree)
    (sum (filter #(< % 100000) (map second tree)))
    )
  )

; (println (build-tree input))
(pp/pprint (-> input build-tree sum-sizes totals))
; (part1 t1)
(part1 input)


(into {} t3)

(defn part2 [in]
  (let [tree (-> in build-tree sum-sizes totals)
        paths (into {} tree)
        used (paths ["/"])
        unused  (- 70000000 used)
        required (- 30000000 unused)
        sizes (sort (vals paths))
        ]
    ; (println used)
    ; (println unused)
    ; (println required)
    (first (filter #(> % required)  sizes))
    ))


; (part2 t1)
; (part2 input)
; (println (part2 input)) 

; (prn (time (part2 input)))
; (part2 input)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
