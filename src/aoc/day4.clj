(ns aoc.day4
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 4)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        pairs (map #(s/split % #"[,-]") lines)
        ranges (map (fn [ls] (map #(Integer/parseInt %) ls))  pairs)
        ]
  (map #(partition 2 %) ranges)))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

(defn range-contains? [[a b] [x y]]
  (cond
    (and (>= a x) (<= b y)) true
    (and (>= x a) (<= y b)) true
    :else false
    ))

(t/are [i o] (= o (apply range-contains? i))
  [[1 3] [2 3]] true
  [[1 3] [2 5]] false
  [[20 33] [5 7]] false
  [[4 5] [2 7]] true
  )

(defn part1 [input]
  (count (filter #(apply range-contains? %) input)))

; (part1 t1)
; (part1 input)


(defn range-overlaps? [[a b] [x y]]
  (cond
    (and (<= x b) (>= y a)) true
    (and (<= y a) (>= x b))  true
    :else false
    ))

(t/are [i o] (= o (apply range-overlaps? i))
  [[5 7] [7 9]] true
  [[2 8] [3 7]] true
  [[6 6] [4 6]] true
  [[2 6] [4 8]] true

  [[2 4] [6 8]] false
  [[2 3] [4 5]] false
  )

(defn part2 [input]
  (count (filter #(apply range-overlaps? %) input)))


; (part2 t1)
; (part2 input)

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
