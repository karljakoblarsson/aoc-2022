(ns aoc.day5
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

(def day 5)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn partition-by-empty-line [lst]
  (remove #(every? empty? %) (partition-by empty? lst)))

(defn get-one [s]
  (let [len (count s)
        idx (set (range 1 len 4))
        f (fn [i a] (if (contains? idx i) a))
        ]
    (keep-indexed f s)))

(defn get-stacks [lst]
  (map get-one lst))


(defn parse-inst [s]
  (let [
        s' (s/replace s #"move " "")
        s'' (s/replace s' #" from " ".")
        s''' (s/replace s'' #" to " ".")
        inst' (s/split s''' #"\.")
        inst'' (map #(Integer/parseInt (str %)) inst')
        inst''' {:move (first inst'') :from (second inst'')  :to (nth inst'' 2)}
        ]
    inst'''
    )
  )
; (parse-inst "move 16 from 8 to 4")
; (parse-inst "move 1 from 9 to 2")

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        [stacks inst] (partition-by-empty-line lines)
        stacks' (map #(s/split % #"") stacks)
        stacks'' (get-stacks stacks')
        inst' (map parse-inst inst)
        ]
    { :stacks (drop-last stacks'')  :inst inst'}
    ))



(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(defn transpose [m]
  (apply mapv vector m))

; (def t1 test-input)
; (def s1 (transpose (:stacks t1)))
; (def s2 (mapv (fn [l] (remove #(= " " %) l)) s1))
; (def s3 (mapv (comp vec reverse) s2))
; (def i1 (:inst t1))

(defn unroll [inst]
  (mapcat #(repeat (:move %) {:from (:from %) :to (:to %)}) inst))

(def i2 (unroll i1))

(defn preprocess-stacks [st]
  (let [
        st' (transpose st)
        st'' (mapv (fn [l] (remove #(= " " %) l)) st')
        st''' (mapv (comp vec reverse) st'')
        ]
    (vec st''')
    ))

(defn preprocess-inst [i]
  (unroll i))

(defn step-one [st inst ]
  (let [fi (- (:from inst) 1)
        ti (- (:to inst) 1)
        element (peek (nth st fi))
        ]
    (if (nil? element)
      st
      (-> st
          (update fi pop)
          (update ti #(conj % element ))
          )
      )
    ))

(defn step-all [state steps]
  (reduce step-one state steps)
  )

; (step-all (vec s3) i2)
; (step-all (preprocess-stacks (:stacks t1)) (preprocess-inst (:inst t1)))
(defn top [st]
  (map peek st))

; (top (step-all (preprocess-stacks (:stacks t1)) (preprocess-inst (:inst t1))))

(defn part1 [{:keys [stacks inst]}]
  (let [stacks' (preprocess-stacks stacks)
        inst' (preprocess-inst inst)
        ]
    (apply str (top (step-all stacks' inst')))
    ))

; (part1 t1)
; (part1 input)
; (println (part1 input))


(defn step-one2 [st inst ]
  (let [fi (- (:from inst) 1)
        ti (- (:to inst) 1)
        move (:move inst)
        elements (take move (nth st fi))
        ]
    (-> (mapv vec st)
        (update fi #(drop move %))
        (update ti #(concat elements %))
        )))

(defn step-all2 [state steps]
  (reduce step-one2 state steps))

(defn preprocess-stacks2 [st]
  (let [ st' (transpose st)
        st'' (map (fn [l] (remove #(= " " %) l)) st')
        ]
    st''))

(defn top2 [st]
  (map first st))

(step-one2 (preprocess-stacks2 (:stacks t1)) (first (:inst t1)) )
(step-all2 (preprocess-stacks2 (:stacks t1)) (:inst t1))

(defn part2 [{:keys [stacks inst]}]
  (let [stacks' (preprocess-stacks2 stacks)
        ]
    (apply str (top2 (step-all2 stacks' inst)))
    ))


; (part2 t1)
; (part2 input)
; (print (part2 input)) 

; (prn (time (part2 input)))
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
