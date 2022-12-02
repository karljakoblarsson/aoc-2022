(ns aoc.day0
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

(def day 0)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(print testfile)
(defn prepare-input [str-input]
  (map (fn [x] (Integer/parseInt x)) (s/split-lines str-input))
  )


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

(rest t1)

(def lt (fn [a b] (if (> b a) 1 0)))

(defn part1 [input]
  (sum (map lt input (rest input)))
  )

; (part1 input)
; (println (time (part1 input)))


(defn sums [in] (map + in (rest in) (rest (rest in) )))

(defn part2 [input]
  (part1 (sums input) ))

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
