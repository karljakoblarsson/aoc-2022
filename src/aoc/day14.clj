(ns aoc.day14
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

(def day 14)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        ]
    (map
      (fn [l] (read-string
                (str "["
                   (-> l
                     (s/replace #"^" "[")
                     (s/replace #"$" "]")
                     (s/replace #"," " ")
                     (s/replace #"->" "] [")
                   )
                   "]")))
     lines)
    ))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
t1

(def start-point [500 0])

(defn line-to-points [[[ax ay] [bx by]]]
  (cond
    (= ax bx) (map (fn [i] [ax i]) (range (min ay by) (inc (max ay by))))
    (= ay by) (map (fn [i] [i ay]) (range (min ax bx) (inc (max ax bx))))
    )
  )

; t1
; (map #(line-to-points (partition 2 1 %)) t1)
; (line-to-points [[503 4] [502 4]])

; (line-to-points (first (partition 2 1 (first t1))))
; (partition 2 1 (first t1))

; (println (map #(partition 2 1 %) t1))

(defn lines-to-points [ls]
  (into {}
        (map (fn [p] [p :r])
             (reduce concat
                     (mapcat #(map line-to-points %)
                             (map #(partition 2 1 %)
                                  ls))))))

(def t2 (lines-to-points t1))

(defn out-of-bounds [in]
  (reduce max (map second (keys in))))

(defn move-sand [out-of-bounds-y stuff [x y]]
  (let [iy (inc y)
        ld (contains? stuff [(dec x) iy])
        d (contains? stuff [x iy])
        rd (contains? stuff [(inc x) iy])
        ]
  (cond
    (and rd d ld (= y 0)) (reduced (assoc stuff [x y] :s))
    (> y out-of-bounds-y) (reduced stuff)
    (and rd d ld) (assoc stuff [x y] :s)
    (and d (not ld)) (recur out-of-bounds-y stuff [(dec x) iy])
    (and d (not rd)) (recur out-of-bounds-y stuff [(inc x) iy])
    :default (recur out-of-bounds-y stuff [x iy])
    )
    )
  )

; (move-sand 9 t2 start-point)
; (out-of-bound t2)
; (map println (partition 2 1 t1))

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
