(ns aoc.day6
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

(def day 6)
(def infile (slurp (mk-input-filename day)))
; (def input (slurp (mk-test-input-filename day)))
(def t1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn prepare-input [in] in)

(defn part1 [input]
  (reduce-kv
    (fn [chrs idx itm]
     (let [ ]
       (if (apply distinct? chrs)
         (reduced (+ 4 idx))
         (conj (vec (rest chrs)) itm)
         )
       )
     )
    (vec (take 4 input))
    (vec (drop 4 input)))
     )

; (println (take 1 t1))


(t/are [i o] (= (part1 i) o)
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7
  "bvwbjplbgvbhsrlpgdmjqwftvncz" 5
  "nppdvjthqldpwncqszvftbrmjlhg" 6
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11
  )


; (part1 infile)

(defn part2 [input]
  (reduce-kv
    (fn [chrs idx itm]
     (let [ ]
       (if (apply distinct? chrs)
         (reduced (+ 14 idx))
         (conj (vec (rest chrs)) itm)
         )
       )
     )
    (vec (take 14 input))
    (vec (drop 14 input)))
     )

(t/are [i o] (= (part2 i) o)
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 19
  "bvwbjplbgvbhsrlpgdmjqwftvncz" 23
  "nppdvjthqldpwncqszvftbrmjlhg" 23
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 29
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 26
  )


; (part2 infile)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
