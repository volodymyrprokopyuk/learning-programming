#lang racket/base
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

(struct student (name id# dorm))

(define vlad (student 'Vlad 1 'dorm1))
(define lana (student 'Lana 2 'dorm2))

(student-name vlad)
(student-id# vlad)
(student-dorm vlad)

;; (define students (list vlad lana))
;; (student-name (second students))
