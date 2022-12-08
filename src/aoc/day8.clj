(ns aoc.day8
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

(def day 8)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        chrs (map #(s/split % #"") lines)
        heights (map (fn [l] (map #(Integer/parseInt %) l))  chrs)
        ]
  heights))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
(println t1)

(defn transpose [m]
  (apply mapv vector m))


(defn visible [idxfn rc otheridx]
  (:visible
   (reduce-kv
    (fn [acc i e]
               (if (> e (:height acc))
                 (-> acc
                     (update :visible #(conj % (idxfn otheridx i)))
                     (assoc :height e))
                 acc
                 ))
             {:height -1 :visible nil}
             (vec rc))
   )
  )


(defn total [in]
  (let [rows in
        cols (transpose in)
        end-idx-rows (dec (count (first rows)))
        end-idx-cols (dec (count (first cols)))
        rowfn (fn [o i] [i o])
        rrowfn (fn [o i] [(- end-idx-rows i) o])
        colfn (fn [o i] [o i])
        rcolfn (fn [o i] [o (- end-idx-cols i)])

        visible-rows (map-indexed #(visible rowfn %2 %1) rows)
        visible-rrows (map-indexed #(visible rrowfn (reverse %2) %1) rows)
        visible-cols (map-indexed #(visible colfn %2 %1) cols)
        visible-rcols (map-indexed #(visible rcolfn (reverse %2) %1) cols)

        visible-rows' (reduce #(into %1 %2) #{}  visible-rows)
        visible-rrows' (reduce #(into %1 %2) #{}  visible-rrows)
        visible-cols' (reduce #(into %1 %2) #{}  visible-cols)
        visible-rcols' (reduce #(into %1 %2) #{}  visible-rcols)
        ]
    (println visible-rows)
    (println visible-rrows)
    (println visible-cols)
    (println visible-rcols)
    (st/union
           visible-rows'
           visible-rrows'
           visible-cols'
           visible-rcols'
           )
    ))

(defn part1 [in]
  (count (total in)))

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
