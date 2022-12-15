(ns aoc.day15
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
   ; [clojure.data.priority-map :as pm]
   )
  (:use aoc.core))

(def day 15)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        edn (map (fn [l] (str "["
                  (-> l
                      (s/replace #"^Sensor at x=" "{ :sensor [")
                      (s/replace #"y=" "")
                      (s/replace #": closest beacon is at x=" "] :beacon [")
                      (s/replace #"$" "]}")
                      )
                  "]"))
             lines)
        ]
    (println edn)
    (map read-string edn)
    ))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; t1

(defn manhattan [[ax ay] [bx by]]
  (+ (abs (- ax bx)) (abs (- ay by))))

; (manhattan [2 -4] [3 2])

(defn all-points [{:keys [sensor beacon]}]
  (let [dist (manhattan sensor beacon)
        [x y] sensor
        ]
    (set (mapcat
      (fn [i] (let [width (abs (- (abs i) dist)) ]  (map (fn [j] [j (+ y i)]) (range (- x width) (inc (+ x width)) ))))
      (range (- 0 dist) (inc (+ 0 dist)) ))) 
    ))

(count (sort (all-points {:sensor [ 0 0] :beacon [2 2]})))

(def t2 (reduce (fn [acc l] (into acc (all-points l))) (set []) (flatten t1)))

(count (set (map first (filter #(= 10 (second %)) t2))) )  

; (flatten t1)

(defn part1 [in]
  (let [points (lines-to-points in)]
    (count
      (filter
      #(= :s %)
      (vals
        (reduce
          (partial move-sand (out-of-bounds points))
          points
          (repeat [500 0])))) )
    )
)

(part1 input)
; (time (part1 input))

(t/are [i o] (= o (part1 i))
  t1 24
  input 779
  )

; (part1 t1)
; (part1 input)

(defn part2 [in]
  (let [points (lines-to-points in)
        max-y (out-of-bounds points)
        floor-y (+ 2 max-y)
        floor-line [[(- 500 (+ 10 floor-y)) floor-y] [(+ 500 floor-y 11) floor-y]]
        points' (lines-to-points (conj in floor-line))
        ]
    ; (println points')
    (count
      (filter
      #(= :s %)
      (vals
        (reduce
          (partial move-sand (out-of-bounds points'))
          points'
          (repeat [500 0])))) )
    )
)

(part2 t1)
; (part2 input)
; (println (part2 input))

(t/are [i o] (= o (part2 i))
  t1 93
  input 27426
  )

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
