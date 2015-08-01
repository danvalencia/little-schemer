(ns little-schemer.core
  (:gen-class))

(defn car
  "Synonym for first"
  [l]
  (first l))

(defn cdr
  "Synonym for rest"
  [l]
  (rest l))

(defn atom?
  "Returns true if argument is not a list, otherwise returns false"
  [arg]
  (not (list? arg)))

(defn eq?
  "Synonym for ="
  [a b]
  (= a b))

(defn null?
  "Synonym for empty?"
  [l]
  (empty? l))

(defn lat?
  "Returns true if neither element in the given array is a list"
  [l]
  (loop [l l]
    (cond
      (empty? l) true
      (atom? (car l)) (recur (cdr l))
      :else false)))
