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

(defn left2 [left right]
  (cond
    (and (int? left) (int? right)) (compare left right)
    (int? left)  (recur [left] right)
    (int? right) (recur left [right])
    (and (empty? left) (empty? right)) 0
    (empty? left) -1
    (empty? right) 1
    :else (let [fr (left2 (first left) (first right))]
            (case fr
              0 (recur (rest left) (rest right))
              -1 -1
              1 1
              )
            )

    )
  )
(defn check2 [{:keys [left right]}]
  (case (left2 left right)
    -1 true
    1 false
    0 nil
   )
  )

; (println (map check2 input))
(t/is (=
       (map check2 t1)
       [true true false true false true false false]))


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

; (pp/pprint (map vector input (map check2 input)))
; (keep-indexed #(if (check2 %2) (inc %1 ) nil) input) 

(defn part1 [in]
  (sum (keep-indexed #(if (check2 %2) (inc %1 ) nil) in)))

; (part1 input)
; (time (part1 input))

(t/are [i o] (= o (part1 i))
  t1 13
  input 5825
  )

; (part1 t1)
; (part1 input)

(defn concat-all [in]
  (reduce (fn [acc {:keys [left right]}] (into acc [left right])) [] in))

; (apply * (keep-indexed #(if (or (= %2 [[2]]) (= %2 [[6]])) (inc %1) nil) (sort left2 (into (concat-all t1) [[[2]] [[6]]]) )))


(defn part2 [in]
  (apply *
         (keep-indexed
          #(if (or (= %2 [[2]]) (= %2 [[6]])) (inc %1) nil)
          (sort left2 (into (concat-all in) [[[2]] [[6]]])))))

; (println (part2 input))
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
