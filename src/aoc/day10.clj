(ns aoc.day10
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.core.match :as match]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.matrix.selection :as sel]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 10)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-line [l]
  (match/match l
    ["addx" d] { :inst :addx :value (Integer/parseInt d)}
    ["noop"] { :inst :noop }
    ))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        chrs (map #(s/split % #" ") lines)
        ]
  (map parse-line chrs)))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

(defn step [x { :keys [inst value]}]
  (let [prev (last x)]
    (case inst
      :addx [ prev (+ prev value)]
      :noop [prev]
      )))

(defn walk [steps]
  (flatten (reductions step [1] steps)))

(defn signal-strengths [xs]
  (map * xs (range 1 (count xs))))

(defn is-right-index [i]
  (= 0 (mod (- i 20) 40)))

(t/are [i o] (= o (is-right-index i))
  0 false
  20 true
  60 true
  100 true
  19 false
  21 false
  120 false
  40 false
  )

(defn to-count [xs]
  (keep-indexed
   (fn [i v] (if (is-right-index (inc i)) v) )
   xs))

(defn part1 [in]
  (-> in
      walk
      signal-strengths
      to-count
      sum
      ))

(t/are [i o] (= o (part1 i))
  t1 13140
  input 14860
  )

; (part1 t1)
; (part1 input)

(defn to-draw [x i]
  (case (- i x)
    -1 "#"
    0 "#"
    1 "#"
    "."
    ))

(defn part2 [in]
  (let [xs (walk in)
        rows (partition 40 xs)
        pixels (map #(map to-draw % (range 0 40)) rows)]
    (dorun (map println pixels))
    ))

; (part2 t1)
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
