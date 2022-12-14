(ns aoc.day9
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.core.match :as match]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.selection :as sel]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 9)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-line [l]
  (match/match l
    ["U" d] { :dir :up :step (Integer/parseInt d)}
    ["D" d] { :dir :down :step (Integer/parseInt d)}
    ["R" d] { :dir :right :step (Integer/parseInt d)}
    ["L" d] { :dir :left :step (Integer/parseInt d)}
    ))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        chrs (map #(s/split % #" ") lines)
        ]
  (map parse-line chrs)))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (println t1)


(defn unroll [inst]
  (mapcat #(repeat (:step %) { :dir (:dir %) :step 1 }) inst))

(def t2 (unroll t1))

(defn update-head [[fx fy] { dir :dir }]
  (case dir
    :up [fx (dec fy)]
    :down [fx (inc fy)]
    :left [(dec fx) fy]
    :right [(inc fx) fy]
    ))

(defn follow [h ot]
  ; (println h)
  ; (println ot)
  (let [delta (mapv - h ot)
        [ox oy] ot
        ]
    ; (print delta)
    (match/match delta
      [0 0]   [ox oy]
      [0 -1]  [ox oy]
      [0 1]   [ox oy]
      [-1 0]  [ox oy]
      [-1 -1] [ox oy]
      [-1 1]  [ox oy]
      [1 0]   [ox oy]
      [1 -1]  [ox oy]
      [1 1]   [ox oy]

      [0 2]   [ox (inc oy)]
      [0 -2]  [ox (dec oy)]
      [2 0]   [(inc ox) oy]
      [-2 0]  [(dec ox) oy]

      [-1 -2] [(dec ox) (dec oy)]
      [-2 -1] [(dec ox) (dec oy)]
      [-2 -2] [(dec ox) (dec oy)]

      [1 2]   [(inc ox) (inc oy)]
      [2 1]   [(inc ox) (inc oy)]
      [2 2]   [(inc ox) (inc oy)]

      [-1 2]  [(dec ox) (inc oy)]
      [-2 1]  [(dec ox) (inc oy)]
      [-2 2]  [(dec ox) (inc oy)]

      [2 -1]  [(inc ox) (dec oy)]
      [2 -2]  [(inc ox) (dec oy)]
      [1 -2]  [(inc ox) (dec oy)]
      )
    )
  )

; (walk t2)

(defn step [acc s]
  (let [new-head (update-head (first acc) s)
        followers (reductions follow (conj (rest acc) new-head) )]
    followers
    ))


(defn init [n] (vec (repeat n [0 0])))
(defn walk [n steps]
  (reductions step (init n) steps))

; ..##..
; ...##.
; .####.
; ....#.
; s###..

(defn print6 [{ :keys [head-pos tail-pos]}]
  (let [f (fn [x y] (cond
                      (= [x y] head-pos) "H"
                      (= [x y] tail-pos) "T"
                      (= [x y] [0 0]) "s"
                      true "."
                      ))]
    (println "---")
    (println (apply str (map #(f % -4) (range 0 6))))
    (println (apply str (map #(f % -3) (range 0 6))))
    (println (apply str (map #(f % -2) (range 0 6))))
    (println (apply str (map #(f % -1) (range 0 6))))
    (println (apply str (map #(f % -0) (range 0 6))))
    )
  )

; (map print6 (reductions step {:head-pos [0 0] :tail-pos [0 0]} t2))
; (println (walk t2))
; (count (distinct (walk t2)))

(defn part1 [in]
  (count (distinct (map last (walk 2 (unroll in))))))

; (part1 t1)
; (part1 input)

(t/are [i o] (= o (part1 i))
  t1 13
  input 6197
  )

; betwen 6279 and 5000


(defn part2 [in]
  (count (distinct (map last (walk 10 (unroll in))) )))

; (part2 t1)
; (part2 input)

(t/are [i o] (= o (part2 i))
  t1 1
  input 2562
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
