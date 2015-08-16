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

(def add1
  (fn [a]
    (inc a)))

(def sub1
  (fn [a]
    (dec a)))

(def sum
  (fn [a b]
    (cond
      (zero? b) a
      :else (add1 (sum a (sub1 b))))))

(def sub
  (fn [a b]
    (cond
      (zero? b) a
      :else (sub (sub1 a) (sub1 b)))))

(def addtup
  (fn [tup]
    (cond
      (null? tup) 0
      :else (+ (car tup) (addtup (cdr tup))))))

(def ×
  (fn [n m]
    (cond
      (= 0 m) 0
      :else (+ n (× n (sub1 m))))))

(def tup+
  (fn [tup1 tup2]
    (cond
      (null? tup1) tup2
      (null? tup2) tup1
      :else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(def >
  (fn [n m]
    (cond
      (zero? n) false
      (zero? m) true
      :else (> (sub1 n) (sub1 m)))))

(def <
  (fn [n m]
    (cond
      (zero? m) false
      (zero? n) true
      :else (< (sub1 n) (sub1 m)))))

(def =
  (fn [n m]
    (cond
      (> n m) false
      (< n m) false
      :else true)))

(def ↑
  (fn [n m]
    (cond
      (zero? m) 1
      :else (× n (↑ n (sub1 m))))))

(def ÷
  (fn [n m]
    (cond
      (< n m) 0
      :else (add1 (÷ (- n m) m)))))

(def length
  (fn [lat]
    (cond
      (null? lat) 0
      :else (add1 (length (cdr lat))))))

(def pick
  (fn [n lat]
    (cond
      (zero? (sub1 n)) (car lat)
      :else (pick (sub1 n) (cdr lat)))))

(def rempick
  (fn [n lat]
    (cond
      (zero? (sub1 n)) (cdr lat)
      :else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(def no-nums
  (fn [lat]
    (cond
      (null? lat) '()
      (number? (car lat)) (no-nums (cdr lat))
      :else (cons (car lat) (no-nums (cdr lat))))))

(def all-nums
  (fn [lat]
    (cond
      (null? lat) '()
      (number? (car lat)) (cons (car lat) (all-nums (cdr lat)))
      :else (all-nums (cdr lat)))))

(def eqan?
  (fn [a1 a2]
    (cond
      (and (number? a1) (number? a2)) (= a1 a2)
      (or (number? a1) (number? a2)) false
      :else (eq? a1 a2))))

(def occur
  (fn [a lat]
    (cond
      (null? lat) 0
      (eqan? a (car lat)) (add1 (occur a (cdr lat)))
      :else (occur a (cdr lat)))))

(def one?
  (fn [n]
    (= n 1)))

(def rempick
  (fn [n lat]
    (cond
      (one? n) (cdr lat)
      :else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(def rember*
  (fn [a l]
    (cond
      (null? l) '()
      (atom? (car l))
        (cond
          (eq? a (car l)) (rember* a (cdr l))
          :else (cons (car l) (rember* a (cdr l))))
      :else (cons (rember* a (car l)) (rember* a (cdr l))))))

(def insertR*
  (fn [new old l]
    (cond
      (null? l) '()
      (atom? (car l))
        (cond
          (eq? old (car l)) (cons old (cons new (insertR* new old (cdr l))))
          :else (cons (car l) (insertR* new old (cdr l))))
      :else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

(def occur*
  (fn [a l]
    (cond
      (null? l) 0
      (atom? (car l))
        (cond
          (eq? a (car l)) (add1 (occur* a (cdr l)))
          :else (occur* a (cdr l)))
      :else (+ (occur* a (car l)) (occur* a (cdr l))))))

(def subst*
  (fn [new old l]
    (cond
      (null? l) '()
      (atom? (car l))
        (cond
          (eq? old (car l)) (cons new (subst* new old (cdr l)))
          :else (cons (car l) (subst* new old (cdr l))))
      :else (cons (subst* new old (car l)) (subst* new old (cdr l))))))

(def insertL*
  (fn [new old l]
    (cond
      (null? l) '()
      (atom? (car l))
        (cond
          (eq? old (car l)) (cons new (cons old (insertL* new old (cdr l))))
          :else (cons (car l) (insertL* new old (cdr l))))
      :else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))

(def member*
  (fn [a l]
    (cond
      (null? l) false
      (atom? (car l))
        (or (eq? (car l) a) (member* a (cdr l)))
      :else (or (member* a (car l)) (member* a (cdr l))))))

