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
      [0 0] [ox oy]
      [0 -1] [ox oy]
      [0 -2] [ox (dec oy)]
      [0 1] [ox oy]
      [0 2] [ox (inc oy)]
      [-1 0] [ox oy]
      [-1 -1] [ox oy]
      [-1 -2] [(dec ox) (dec oy)]
      [-1 1] [ox oy]
      [-1 2] [ox (inc oy)]
      [-2 0] [(dec ox) oy]
      [-2 -1] [(dec ox) (dec oy)]
      [-2 -2] [(dec ox) (dec oy)]
      [-2 1] [(dec ox) (inc oy)]
      [-2 2] [(dec ox) (inc oy)]
      [1 0] [ox oy]
      [1 -1] [ox oy]
      [1 -2] [(inc ox) (dec oy)]
      [1 1] [ox oy]
      [1 2] [(inc ox) (inc oy)]
      [2 0] [(inc ox) oy]
      [2 -1] [(inc ox) (dec oy)]
      [2 -2] [(inc ox) (dec oy)]
      [2 1] [(inc ox) (inc oy)]
      [2 2] [(inc ox) (inc oy)]
      )
    )
  )

(walk t2)

(defn step [{:keys [head-pos tail-pos] :as acc} s]
  ; (println acc s)
  (let [new-head (update-head head-pos s)]
    { :head-pos new-head :tail-pos (follow new-head tail-pos)}
    ))

; (update-head [0 [0]])
; (step {:head-pos [0 0] :tail-pos [0 0]} (first t2))

(defn walk [steps]
  (map :tail-pos
       (reductions step {:head-pos [0 0] :tail-pos [0 0]} steps)
       ))

; ..##..
; ...##.
; .####.
; ....#.
; s###..

(defn print5 [head-po tail-pos]
  
  )

(println (walk t2))
(count (distinct (walk t2)))

(defn part1 [in]
  (count (distinct (walk (unroll in)))))

(part1 t1)
(part1 input)

; TODO Fix tihs
(defn count-visible-line [line]
  (:count
  (reduce
    (fn [acc e]
      (cond
        (>= e (:height acc)) (reduced (update acc :count inc))
        (< e (:height acc)) (update acc :count inc)
        )
      )
    { :height (first line) :count 0}
    (rest line))
   ))

(t/are [i o] (= (count-visible-line i) o)
       [5 3] 1
       [5 5 2] 1
       [5 1 2] 2
       [5 3 5 3] 2

       [5 3 5 3] 2
       [5 3 3] 2
       [5 3] 1
       [5 4 9] 2
       )

(defn calc-scenic [mtr [r c]]
  (let [rend (- (m/row-count mtr) 1)
        cend (- (m/column-count mtr) 1)
        row (count-visible-line
              (sel/sel mtr (sel/irange r rend 1) c))
        rrow (count-visible-line
               (sel/sel mtr (sel/irange r 0 -1) c))
        col (count-visible-line
              (sel/sel mtr r (sel/irange c rend 1)))
        rcol (count-visible-line
               (sel/sel mtr r (sel/irange c 0 -1)))
        ]
    (* row rrow col rcol)
    ))

(t/are [i o] (= o (calc-scenic (m/matrix t1) i))
  [1 2] 4
  [3 2] 8
  )

(defn all-scenic [mtr]
  (m/emap-indexed (fn [coord _] (calc-scenic mtr coord)) mtr))

(defn part2 [in]
  (m/emax (all-scenic (m/matrix in))))


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
