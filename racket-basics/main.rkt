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
;; (all-but-last '(1))
;; (all-but-last '(1 2))

;; Recursive map
(define (my-map fun lst)
  (cond [(empty? lst) empty]
        [else (cons (fun (first lst)) (my-map fun (rest lst)))]))

;; (my-map add1 '(1 2 3 4))
;; Lambda function
;; (my-map (lambda (num) (+ num 10)) '(1 2 3 4))
;; Immediagely invoked function expression
;; ((lambda (num) (* num 10)) 1)

;; Recursive filter
(define (my-filter prd? lst)
  (cond [(empty? lst) empty]
        [(prd? (first lst))
         (cons (first lst) (my-filter prd? (rest lst)))]
        [else (my-filter prd? (rest lst))]))

;; (my-filter even? '(1 2 3 4 5 6 7))

(define (my-any prd? lst)
  (cond [(empty? lst) #f]
        [else (or (prd? (first lst)) (my-any prd? (rest lst)))]))

;; (my-any odd? '(2 4 6))
;; (my-any odd? '(2 4 5 6))

(define (my-all prd? lst)
  (cond [(empty? lst) #t]
        [else (and (prd? (first lst)) (my-all prd? (rest lst)))]))

;; (my-all odd? '(1 2 3 4 5))
;; (my-all odd? '(1 3 5))

(define (my-foldr fun int lst)
  (cond [(empty? lst) int]
        [else (fun (first lst) (my-foldr fun int (rest lst)))]))

;; (my-foldr + 0 '(1 2 3 4 5))
;; (my-foldr cons empty '(a b c))

(define (my-foldl fun int lst)
  (cond [(empty? lst) int]
        ;; new init value
        [else (my-foldl fun (fun (first lst) int) (rest lst))]))

;; (my-foldl + 0 '(1 2 3 4 5))
;; (my-foldl cons empty '(a b c))

(define (my-build-list fun len)
  (define (builder cur)
    (cond [(= cur len) empty]
          [else (cons (fun cur) (builder (add1 cur)))]))
  (builder 0))

;; (my-build-list add1 10)
;; (my-build-list (lambda (num) (* num 10)) 10)

(define (d/dx fun)
  (define d (/ 1 100000))
  (lambda (x) (/ (- (fun (+ x d)) (fun (- x d))) 2 d)))

(define 2x/dx (d/dx (lambda (x) (* 2 x))))

;; (map 2x/dx '(-10 -5 0 3 7 14))

;; Apply function
(define (sum lst)
  (apply + lst))

;; (sum '(1 2 3 4 5))

(define (highest lst)
  (apply max lst))

;; (highest '(1 4 -2 6 7 10))

;; Structure inheritance
(struct parent ([knowledge #:mutable]) #:transparent) ; per field mutability
(struct child parent (health) #:mutable #:transparent) ; whole structure mutable

(define a-child (child 'a-knowledge 'a-health))
;; a-child
;; (parent-knowledge a-child)
;; (child-health a-child)
;; Mutate structures
;; (set-parent-knowledge! a-child 'new-knowldege)
;; (set-child-health! a-child 'new-health)
;; a-child

(define (update-struct! mutator)
  (lambda (str val)
    (mutator str val)))

(define parent-knowledge!
  (update-struct! set-parent-knowledge!))

(define child-health!
  (update-struct! set-child-health!))

;; (parent-knowledge! a-child 'updated-knowledge)
;; (child-health! a-child 'updated-health)
;; a-child

;; (let sets up local definitions)
;; (equal?
;;  (let ((ch (child 'knowledge 'health)))
;;    (set-child-health! ch 'mutated-health)
;;    (child-health ch))
;;  'mutated-health)
