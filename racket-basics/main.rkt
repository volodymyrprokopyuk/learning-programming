#lang racket
(require racket)
;; 1. racket -f main.rkt -i
;; 2. (load "main.rkt")

(define lower 0)
(define upper 100)

(define (start m n)
  (set! lower (min m n))
  (set! upper (max m n))
  (guess))

(define (guess)
  (quotient (+ lower upper) 2))

(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

(define (bigger)
  (set! lower (min upper (add1 (guess))))
  (guess))

;; Line comment (beginning of the line)
(define (square n)
  #;(print n) ; s-expression comment
  (* n n)) ; square n (inline comment)

;; opaque structure (by default)
(struct student (name id# dorm) #:transparent) ; transparent structure

(define vlad (student 'Vlad 1 'dorm1))
(define lana (student 'Lana 2 'dorm2))
;; (student-name vlad)
;; (student-id# vlad)
;; (student-dorm vlad)
;; (student? vlad)

(define students (list vlad lana))
;; (student-name (second students))

(define (is-number-equal x y)
  (if (= x y)
      'equal
      'not-equal))

(define (is-even-odd x)
  (cond [(> x 100) 'big-number]
        [(even? x) 'even]
        [else 'odd]))

(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))

;; and, or are conditional forms + shortcut boolean evaluation
(define is-odd #f)
;; (and (odd? 1) (set! is-odd #t))
;; (or (odd? 1) (set! is-odd #t))

(define (contains? value a-list)
  (if (member value a-list)
      'contains
      'does-not-contain))

(struct point (x y))

(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))

(define p1 (point 3 4))
;; (distance-to-origin p1)

;; equal? -> value equality
;; Recursive definition of equal? for every data type (dynamically added)
(define (my-equal? a b)
  (cond [(and (point? a) (point? b)) ; type check
         (and (my-equal? (point-x a) (point-x b)) ; recursive value check
              (my-equal? (point-y a) (point-y b)))]
        [(and (number? a) (number? b))
         (= a b)]
        [else #f]))

(define p2 (point 1 2))
(define p3 (point 3 4))
;; (my-equal? p1 p3)

;; Alias for a variable
(define p4 p1)
;; eq? instance equality
;; (eq? p1 p3) ; #f
;; (eq? p1 p4) ; #t

;; (error 'main "Oh")

;; Local definitions
(define (my-max lst dft)
  (cond [(empty? lst) (list dft)]
        [else
         (define fst (first lst))
         (define rst (rest lst))
         (if (> fst dft) (my-max rst fst) (my-max rst dft))]))

;; (my-max '() 0)
;; (my-max '(10 20 30 40) 10)
;; (my-max '(1 3 5 4 7 2 0) 0)

;; Works only on non-empty list
(define (all-but-last lst)
  (cond [(empty? (rest lst)) empty]
        [else (cons (first lst) (all-but-last (rest lst)))]))

;; (all-but-last '()) ; error
(all-but-last '(1))
(all-but-last '(1 2))
