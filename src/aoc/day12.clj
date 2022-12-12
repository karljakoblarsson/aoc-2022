(ns aoc.day12
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.core.match :as match]
   [clojure.core.matrix :as m]
   ; [clojure.core.matrix.selection :as sel]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   [clojure.data.priority-map :as pm]
   )
  (:use aoc.core))

(def day 12)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn height [c]
  (- (int c) 96))

(t/are [i o] (= o (height i))
       \a 1
       \z 26)

(defn prepare-input [str-input]
  (let [
        single-line (s/replace str-input #"\r\n" "")
        start-index (s/index-of single-line "S")
        end-index (s/index-of single-line "E")
        str-in' (s/replace str-input #"S" "a")
        str-in'' (s/replace str-in' #"E" "z")
        lines (s/split-lines str-in'')
        columns (count (first lines))
        start-column (mod start-index columns)
        end-column (mod end-index columns)
        start-row (quot start-index columns)
        end-row (quot end-index columns)
        heights (map #(map height %) lines)
        ]
  { :grid (m/matrix heights) :start [start-row start-column] :end [end-row end-column ]}
    ))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

; ((:operation (nth  t1 1)) 2) 

(defn heuristic [[mr mc] [r c]]
  (let [dr (- mr r)
        dc (- mc c)]
    (Math/sqrt (+ (* dr dr) (* dc dc)))))

(defn get-neig [rows cols [r c]]
  (let [f (fn [[x y]] (and (>= x 0) (< x rows) (>= y 0) (< y cols)))
        deltas [[-1 0] [1 0] [0 -1] [0 1]]
        coords (map (fn [[x y]] [(+ r x) (+ c y)]) deltas) ]
    (filter f coords)))


(defn check-neigh [dists mr mc [open came-from] current-g [r c :as p] end-pos current]
  (let [curr-d 1
        d-neigh (m/mget dists r c)
        tent-g (+ current-g curr-d)
        ]
    (if (<= tent-g d-neigh)
      (do
        (m/mset! dists r c tent-g)
        [(assoc open p (+ tent-g (heuristic end-pos p)))
         (assoc came-from p current) 
         ] )
      [open came-from]
      )))

(def large-value 99999)
(defn zm [r c [sr sc]]
  (m/mset (m/emap int (m/fill (m/zero-matrix r c) large-value)) sr sc 0))

; function reconstruct_path(cameFrom, current)
    ; total_path := {current}
    ; while current in cameFrom.Keys:
    ;     current := cameFrom[current]
    ;     total_path.prepend(current)
    ; return total_path
(defn reconstruct-path [came-from end]
  ; (println came-from)
  ; (println end)
  (loop [path []
         current end ]
    (if (not (contains? came-from current)) 
      path
      (recur (conj path (came-from current)) (came-from current))
      )))

; (println (:cost (run-astar t1)))


(defn astar [heights dists rows cols open current end-pos came-from]
  ; (println "current" current)
  (let [[r c] current
        dists' (m/ensure-mutable dists)
        neig (get-neig rows cols current)
        current-height (m/mget heights r c)
        neig' (filter (fn [[nr nc]] (<= (- (m/mget heights nr nc) current-height) 1)) neig)
        current-g (m/mget dists r c)
        open' (pop open)
        [open'' came-from'] (reduce
                #(check-neigh dists' rows cols %1 current-g %2 end-pos current)
                [open' came-from]
                neig')]
    ; (println "neig: " neig')
    ; (println "open: " open)
    ; (println "open'': " open'')
    (cond
      (nil? current) :failure
      (= end-pos current) { :path (reverse (reconstruct-path came-from' current))  :cost (m/mget dists r c)} 
      :else (recur
         heights
         dists'
         rows
         cols
         open''
         (first (peek open''))
         end-pos
         came-from'
             ))))

; (prn (astar t1 (zm 10 10) 10 10 (pm/priority-map [0 0] 0) [0 0]))

(defn run-astar [{ :keys [grid start end]} ]
  (let [rows (m/dimension-count grid 0)
        cols (m/dimension-count grid 1)
        init-dists (zm rows cols start)
        open (pm/priority-map start 0)
        ]
    ; (println "end:" end)
    (astar grid init-dists rows cols open start end {})))

(:cost (run-astar input))

(defn part1 [in]
  (apply * (take 2 (reverse (sort (get-inspected (play in)))))
         ))

(part1 t1)

(t/are [i o] (= o (part1 i))
  t1 10605
  input 113232
  )

; (part1 t1)
; (part1 input)

(defn part2 [in]
  (apply * (take 2 (reverse (sort (get-inspected (play2 in)))))
         ))

(println (part2 input))
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
