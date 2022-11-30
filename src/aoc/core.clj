(ns aoc.core)

(defn mk-input-filename [n]
  (str "inputs/day" n ".txt"))

(defn mk-test-input-filename [n]
  (str "inputs/test" n ".txt"))

(defn ifv [pred v f]
  (if (pred v) (f v) v))

(defn call-if [bool f v]
  (if bool (f v) v))

(defn count-pred [pred coll]
  (reduce (fn [acc e] (call-if (pred e) inc acc)) 0 coll))

(defn sum [coll] (reduce +' coll))
