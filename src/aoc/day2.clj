(ns aoc.day2
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   [clojure.core.match :as match]
   )
  (:use aoc.core))

(def day 2)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


 ; (s/split testfile #"\r\n\r")
(defn prepare-input [str-input]
  (let [
        ; lines (s/split str-input #"\r\n")
        lines (s/split-lines str-input)
        pairs (map #(s/split % #" ") lines)
        kvs (map #(map keyword %) pairs)
        ]
    (print pairs)
    kvs))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
t1


(defn your-outcome [pair]
  (match/match (vec pair)
    [:A :X] :draw
    [:A :Y] :win
    [:A :Z] :loose
    [:B :X] :loose
    [:B :Y] :draw
    [:B :Z] :win
    [:C :X] :win
    [:C :Y] :loose
    [:C :Z] :draw
    )
  )

(defn outcome-points [p] (p { :win 6 :draw 3 :loose 0}))
(defn pick-points [[_ p]] (p { :X 1 :Y 2 :Z 3}))
(defn points [p] (+ (pick-points p) (outcome-points (your-outcome p))))

(defn totals [strat] (sum (map points strat)) )
(totals t1)

(defn part1 [input]
  (totals input))

(part1 input)
; (println (time (part1 input)))

(defn your-pick [p] {})

(defn win [x] (x { :A :Y :B :Z :C :X }))
(defn draw [x] (x { :A :X :B :Y :C :Z }))
(defn loose [x] (x { :A :Z :B :X :C :Y }))

(defn pick2 [[a b]]
  (match/match b
    :X [a (loose a)] 
    :Y [a (draw a)] 
    :Z [a (win a)] 
    ))

(map pick2 t1)
(defn totals2 [strat] (sum (map points (map pick2 strat))) )


(defn part2 [input]
  (totals2 input)
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
