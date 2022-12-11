(ns aoc.day11
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

(def day 11)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-monkey [[mn start op mtest mtrue mfalse _]]
  (let [
        number (-> mn (s/replace #"Monkey " "") (s/replace #":" "") (Integer/parseInt)) 
        start-items (-> start
                        (s/replace #"  Starting items: " "[")
                        (s/replace #"$" "]")
                        (read-string))
        op-fn (-> op
                  (s/replace #"  Operation: new = " "")
                  (s/replace #"(.*) ([+*]) (.*)" "($2 $1 $3)")
                  (s/replace #"^" "(fn [old] ")
                  (s/replace #"$" ")")
                  (read-string)
                  (eval)
                  )
        test-fn (-> mtest
                    (s/replace #"  Test: divisible by " "")
                    (read-string))
        to-true (-> mtrue
                    (s/replace #"    If true: throw to monkey " "")
                    (read-string))
        to-false (-> mfalse
                    (s/replace #"    If false: throw to monkey " "")
                    (read-string))
        ]
    {:monkey number
     :items start-items
     :operation op-fn
     :test-num test-fn
     :true-dest to-true
     :false-dest to-false
     :inspected 0 }
    )
  )

; (parse-monkey (take 7 (s/split-lines testfile)))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        ms (partition 7 7 "\n" lines)
        ]
  (map parse-monkey ms)))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
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
