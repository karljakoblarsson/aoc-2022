(ns aoc.day3
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

(def day 3)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        ]
  (map seq lines)))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
(first t1)

(defn halve [l] (vec (partition (/ (count l) 2) l)) )
(first (halve (first t1)))

(defn common [[f s]] (first (filter #(contains? (set f) %) s)))
; (common (halve (first t1)))

; (map #(common (halve %)) t1)

(defn score [c]
 (if (Character/isUpperCase c)
    (+ 26 (- (int c) 64))
    (- (int c) 96)) )

(t/are [i o] (= o (score i))
       \A 27
       \Z 52
       \a 1
       \z 26
       )

(defn part1 [input]
  (sum (map score (map #(common (halve %)) input))))

; (part1 t1)
; (part1 input)


; (map #(map first %) (partition 3 t1))
(defn common3 [[a b c]] (st/intersection (set a) (set b) (set c)))

(defn cmn [lst] (apply st/intersection (map set lst)))

(map #(cmn (vec %)) (partition 3 t1))
; (map (comp first common3) (partition 3 t1) )

(defn part2 [input]
  (sum (map score (map (comp first common3) (partition 3 input)))))

; (part2 input)
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
