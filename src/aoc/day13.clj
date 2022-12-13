(ns aoc.day13
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.core.match :as match]
   [clojure.core.matrix :as m]
   ; [clojure.core.matrix.selection :as sel]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   [clojure.data.priority-map :as pm]
   )
  (:use aoc.core))

(def day 13)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn partition-by-empty-line [lst]
  (remove #(every? empty? %) (partition-by empty? lst))
)

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        pairs (partition-by-empty-line lines)
        ]
    (map (fn [[l r]] {:left (read-string l) :right (read-string r)}) pairs)
    ))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
t1

(defn left-is-smaller [left right]
  (let [l (first left) r (first right)]
    (cond
      (and (int? l) (int? r) (= l r)) (recur (rest left) (rest right))
      (and (int? l) (int? r)) (< l r)
      (and (nil? l) (nil? r)) true
      (nil? l) true
      (nil? r) false
      (and (vector? l) (vector? r)) (and (left-is-smaller l r) (left-is-smaller (rest left) (rest right)))
      (vector? l) (and (left-is-smaller l [r]) (left-is-smaller (rest left) (rest right)))
      (vector? r) (and (left-is-smaller [l] r) (left-is-smaller (rest left) (rest right)))
      (= l r) (recur (rest left) (rest right))
      (< l r) true
      (> l r) false
      :default false
      )
    )
  )

(t/is (=
       (map check-if-left-is-smaller t1)
       [true true false true false true false false]))

; (map check-if-left-is-smaller t1)

(defn check-if-left-is-smaller [{:keys [left right]}]
  (left-is-smaller left right))

(t/are [i o] (= (left-is-smaller (first i) (second i)) o)
  [[] []] true
  [[] [1]] true
  )

(pp/pprint (map vector input (map check-if-left-is-smaller input)))

(defn part1 [in]
  (sum (keep-indexed #(if (check-if-left-is-smaller %2) (inc %1 ) nil) in)))

(keep-indexed #(if (check-if-left-is-smaller %2) (inc %1 ) nil) input) 

(part1 input)
; 720 is too high
; and 500 is to low
; and 710 is to low

; (time (part1 input))

(t/are [i o] (= o (part1 i))
  t1 31
  input 534
  )

; (part1 t1)
; (part1 input)

(defn part2 [in]
  (m/emin (m/emap #(if (= % 0) large-value %) (m/emul (:costs (run-dijkstra in)) (as (:grid in))))))

; (println (part2 input))
(part2 t1)
; lower than 1119
; and lower than 1000

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
