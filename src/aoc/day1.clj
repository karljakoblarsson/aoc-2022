(ns aoc.day1
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 1)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


 (s/split testfile #"\r\n\r")
(print testfile)
(defn prepare-input [str-input]
  (let [
        elves (s/split str-input #"\r\n\r\n")
        strs (map s/split-lines elves)
        is (map (fn [ls] (map #(Integer/parseInt %) (remove nil? ls))) strs)
        ]
    is))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; t1

(rest t1)

(defn totals [lsts] (map sum lsts))
(totals t1)

(defn part1 [input]
  (reduce max (totals input))
  )

(part1 input)
; (println (time (part1 input)))


(defn part2 [input]
  (sum (take 3 (reverse (sort (totals input)))  ))
  )

; (prn (time (part2 input)))
(part2 input)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
