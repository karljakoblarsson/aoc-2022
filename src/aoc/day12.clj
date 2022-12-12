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
        str-in'' (s/replace str-in' #"S" "z")
        lines (s/split-lines str-in'')
        columns (count (first lines))
        start-column (mod start-index columns)
        end-column (mod end-index columns)
        start-row (quot start-index columns)
        end-row (quot end-index columns)
        heights (map #(map height %) lines)
        ]
    (println start-index)
    (println end-index)
    (println columns)
    (println (count lines))
  { :grid (m/matrix heights) :start [start-row start-column] :end [end-row end-column ]}
    ))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
t1
; ((:operation (nth  t1 1)) 2) 

(defn process-monkey [state i]
  (let [{ :keys [items operation test-num true-dest false-dest] :as m} (nth state i)
        worries (map #(quot (operation %) 3) items)
        test-fn #(= 0 (mod % test-num))
        groups (group-by test-fn worries)
        to-true (get groups true)
        to-false (get groups false)
        inspected (count worries)
        new-monkey (-> m
                       (assoc :items [])
                       (update :inspected #(+ % inspected)))
        ]
    (-> state
        (assoc i new-monkey)
        (update-in [true-dest :items] #(concat % to-true))
        (update-in [false-dest :items] #(concat % to-false))
        )
    ))

(println (process-monkey (vec t1) 0))
; (println (map :monkey (vec t1)))

(defn round [state]
  (let [n (count state)]
    (reduce process-monkey (vec state) (range 0 n))
    )
  )

(map :items (round t1))

(defn play [state]
  (nth (iterate round state) 20)
  )


(apply )
(sort (map :inspected (play t1)))

(defn get-inspected [l] (map :inspected l))

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

(defn process-monkey2 [state i]
  (let [{ :keys [items operation test-num true-dest false-dest] :as m} (nth state i)
        common-num (apply * (map :test-num state))
        worries (map #(mod (operation %) common-num) items)
        test-fn #(= 0 (mod % test-num))
        groups (group-by test-fn worries)
        to-true (get groups true)
        to-false (get groups false)
        inspected (count worries)
        new-monkey (-> m
                       (assoc :items [])
                       (update :inspected #(+ % inspected)))
        ]
    (-> state
        (assoc i new-monkey)
        (update-in [true-dest :items] #(concat % to-true))
        (update-in [false-dest :items] #(concat % to-false))
        )
    ))


(defn round2 [state]
  (let [n (count state)]
    (reduce process-monkey2 (vec state) (range 0 n))
    ))

; (println (get-inspected (nth (iterate round2 t1) 10000)))

(defn play2 [state]
  (nth (iterate round2 state) 10000))

; (println (get-inspected (play2 t1)))

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
