(ns little-schemer.core
  (:gen-class))

(def car
  (fn [l]
    (first l)))

(def cdr
  (fn [l]
    (rest l)))

(def atom?
  (fn [a]
    (not (seq? a))))

(def eq?
  (fn [a b]
    (= a b)))

(def null?
  (fn [l]
   (or
    (nil? l)
    (= () l))))

(def lat?
  (fn [l]
    (cond
      (null? l) true
      (atom? (car l)) (lat? (cdr l))
      :else false)))

(def member?
  (fn [a lat]
    (cond
      (null? lat) false
      :else (or (eq? (car lat) a)
                (member? a (cdr lat))))))

(def rember
  (fn [a lat]
    (cond
      (null? lat) '()
      (eq? (car lat) a) (cdr lat)
      :else (cons (car lat)
                  (rember a (cdr lat))))))

(def firsts
  (fn [l]
    (cond
      (null? l) '()
      :else (cons (car (car l))
                   (firsts (cdr l))))))

(def insertR
  (fn [new old lat]
    (cond
      (null? lat) '()
      (eq? (car lat) old) (cons old (cons new (cdr lat)))
      :else (cons (car lat) (insertR new old (cdr lat))))))

(def insertL
  (fn [new old lat]
    (cond
      (null? lat) '()
      (eq? (car lat) old) (cons new lat)
      :else (cons (car lat) (insertL new old (cdr lat))))))

(def subst
  (fn [new old lat]
    (cond
      (null? lat) '()
      (eq? (car lat) old) (cons new (cdr lat))
      :else (cons (car lat) (subst new old (cdr lat))))))

(def subst2
  (fn [new o1 o2 lat]
    (cond
      (null? lat) '()
      (or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))
      :else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(def multirember
  (fn [a lat]
    (cond
      (null? lat) '()
      (eq? a (car lat)) (multirember a (cdr lat))
      :else (cons (car lat) (multirember a (cdr lat))))))

(def multiinsertR
  (fn [new old lat]
    (cond
      (null? lat) '()
      (eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat))))
      :else (cons (car lat) (multiinsertR new old (cdr lat))))))

(def multiinsertL
  (fn [new old lat]
    (cond
      (null? lat) '()
      (eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat))))
      :else (cons (car lat) (multiinsertL new old (cdr lat))))))

(def multisubst
  (fn [new old lat]
    (cond
      (null? lat) '()
      (eq? old (car lat)) (cons new (multisubst new old (cdr lat)))
      :else (cons (car lat) (multisubst new old (cdr lat))))))