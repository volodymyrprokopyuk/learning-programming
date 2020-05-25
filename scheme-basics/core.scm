(use-modules (system vm trace))
(use-modules (ice-9 pretty-print))
(use-modules (srfi srfi-1))

(define pp pretty-print)

;; Line comment
(define (my-square n)
  #;(pp n) ; s-expression comment
  (* n n)) ; inline comment

;; (pp (my-square 0))
;; (pp (my-square 1))
;; (pp (my-square 2))

;; Local bindings
(define (my-max lst dft)
  (if [null? lst]
      dft
      (let ([fst (car lst)]
            [rst (cdr lst)])
        (if [> fst dft] (my-max rst fst) (my-max rst dft)))))

;; (pp (my-max '() 0))
;; (pp (my-max '(1 2 3 4) 1))
;; (pp (my-max '(1 3 5 4 7 2 0) 0))


;; Recursive map
(define (my-map fun lst)
  (if [null? lst]
      '()
      (cons (fun (car lst)) (my-map fun (cdr lst)))))

;; (pp (my-map (lambda (x) (* x 10)) '(1 2 3 4)))

;; Recursive filter
(define (my-filter prd? lst)
  (cond [(null? lst) '()]
        [(prd? (car lst))
         (cons (car lst) (my-filter prd? (cdr lst)))]
        [else (my-filter prd? (cdr lst))]))

;; (pp (my-filter even? '(1 2 3 4 5 6 7)))

(define (my-any prd? lst)
  (if [null? lst]
      #f
      (or (prd? (car lst)) (my-any prd? (cdr lst)))))

;; (pp (my-any odd? '()))
;; (pp (my-any odd? '(2 4 6)))
;; (pp (my-any odd? '(2 4 5 6)))

(define (my-all prd? lst)
  (if [null? lst]
      #t
      (and (prd? (car lst)) (my-all prd? (cdr lst)))))

;; (pp (my-all odd? '()))
;; (pp (my-all odd? '(1 2 3 4 5)))
;; (pp (my-all odd? '(1 3 5)))

(define (my-foldr fun acc lst)
  (if [null? lst]
      acc
      ;; initial accumulator value
      (fun (car lst) (my-foldr fun acc (cdr lst)))))

;; (pp (my-foldr + 0 '()))
;; (pp (my-foldr + 0 '(1 2 3 4 5)))
;; (pp (my-foldr cons '() '(a b c)))

(define (my-foldl fun acc lst)
  (if [null? lst]
      acc
      ;; new accumulator value
      (my-foldl fun (fun (car lst) acc) (cdr lst))))

;; (pp (my-foldl + 0 '()))
;; (pp (my-foldl + 0 '(1 2 3 4 5)))
;; (pp (my-foldl cons '() '(a b c)))

(define (d/dx fun)
  (define d (/ 1 100000))
  (lambda (x) (/ (- (fun (+ x d)) (fun (- x d))) 2 d)))

(define 2x/dx (d/dx (lambda (x) (* 2 x))))

;; (pp (map 2x/dx '(-10 -5 0 3 7 14)))

;; Apply function
(define (my-sum lst)
  (apply + lst))

;; (pp (my-sum '(1 2 3 4 5)))

;; case form
(define (classify num)
  (case num
    ((1 2 3 4 5) 'small)
    ((6 7 8 9) 'mediuml)
    (else 'big)))

;; (pp (classify 2))
;; (pp (classify 7))
;; (pp (classify 15))

;; For side effects only
;; (for-each pp '(a b c d))

;; Lazy evaluation with lambda
(define lazy+ (lambda () (apply + '(1 2 3 4 5))))

;; (pp (lazy+))

;; Memoization is result caching via closure
(define (memoize fun)
  (let ([run? #f]
        [memoized #f])
    ;; (lambda ()
    ;;   (cond [run? memoized]
    ;;         [else (set! run? #t) (set! memoized (fun)) memoized]))))
    (lambda ()
      (if [not run?]
          (begin (set! run? #t) (set! memoized (fun))))
      memoized)))

(define (big-computation)
  ;; (pp 'compute)
  ;; (newline)
  (pp 'compute)
  'result)

(define mbig-computation (memoize big-computation))

;; (pp (mbig-computation))
;; (pp (mbig-computation))

(define (memoize2 fun)
  (define (cache)
    (let ([memoized (fun)])
      (set! cache (lambda () memoized))
      memoized))
  (lambda () (cache)))

(define m2big-computation (memoize2 big-computation))

;; (pp (m2big-computation))
;; (pp (m2big-computation))

;; Optional positional/unnamed parameters + default parameters
;; (pp ((lambda* (a b #:optional c d (f 'f))
;;    (list a b c d f)) 'a 'b 'c 'd))

;; Optional keyword/named parameters + default paramters
;; (pp ((lambda* (a b #:key c d (f 'f))
;;    (list a b c d f)) 'a 'b #:d 'd #:c 'c))

;; Optional rest parameters with cons dot
;; (pp ((lambda (a b . rst)
;;    (list a b rst)) 'a 'b 'c 'd))

;; Optional rest parameters
;; (pp ((lambda* (a b #:rest c)
;;    (list a b c)) 'a 'b 'c 'd))

;; Optional/positional, keword/named, rest, and default parameters: all-in-one
(define* (my-params a #:optional (b 'b) #:key (c 'c) #:rest rst)
  (list a b c rst))

;; Mandatory parameter
;; (pp (my-params 'A))
;; Optional/positional parameter
;; (pp (my-params 'A 'B))
;; Keyword/named parameter
;; (pp (my-params 'A 'B #:c 'C))
;; Rest of parameters
;; (pp (my-params 'A 'B #:c 'C 'R 'Q))

;; Closures
(define* (make-counter #:key (start 0) (step 1))
  (let ([cnt start])
    (lambda ()
      (set! cnt (+ cnt step))
      cnt)))

;; (define counter1 (make-counter))
;; (define counter2 (make-counter #:step 10 #:start 100))
;; (pp (counter1))
;; (pp (counter2))
;; (pp (counter1))
;; (pp (counter1))
;; (pp (counter2))

#|
** Scheme langauge block comment **
|#

(define square (lambda (x) (* x x)))

;; (pp (square 4))

(define reciprocal (lambda (x) (if [= x 0] "oh" (/ 1 x))))

;; (pp (reciprocal 0))
;; (pp (reciprocal 2))

;; Nested let expressions
;; (let ([a 3] [b 4])
;;   (let ([aa (* a a)] [bb (* b b)])
;;     (+ aa bb a b)))

;; Local variable binding shadowing
;; (let ([x 1])
;;   (let ([x 2])
;;     x))

;; Lambda exmpression yield procedure
;; (let ([double (lambda (x) (+ x x))])
;;   (list (double 1) (double 2)))

;; (let ([double-any (lambda (fun x) (fun x x))])
;;   (list (double-any + 1) (double-any cons 'a)))

;; let is implemented in terms of lambda
;; (let ([x 'a]) (cons x x))
;; ((lambda (x) (cons x x)) 'a)

;; lambda > proper list > exact number of parameters (all mandatory)
;; ((lambda (a b) (cons a b)) 'a 'b)
;; lambda > single variable > all parameters as a rest list (all optional)
;; ((lambda rst rst) 'a 'b 'c)
;; lambda > improper list > exact number + rest parameters (mandatory + optional)
;; ((lambda (a b . rst) (list a b rst)) 'a 'b 'c 'd)

;; Top-level lambda definition
(define my-list (lambda x x))

;; (pp (my-list 1 2 3 4))

(define my-cadr (lambda (lst) (car (cdr lst))))

;; (pp (my-cadr '(1 2 3 4)))

(define my-cddr (lambda (lst) (cdr (cdr lst))))

;; (pp (my-cddr '(1 2 3 4)))

;; Top-level lambda definition (abbrieviated)
(define (my-list2 . rst)
  rst)

;; (pp (my-list2))
;; (pp (my-list2 1))
;; (pp (my-list2 1 2))

(define (doubler fun)
  (lambda (x) (fun x x)))

;; (define double+ (doubler +))
;; (pp (double+ 1/2))

;; Procedure can be defined in any order
;; Reference proc2, then define proc2
(define (proc1) (proc2))
(define (proc2) 'proc2-from-proc1)

;; (pp (proc1))

;; Procedure composition (less efficient)
(define (my-compose p1 p2)
  (lambda (x) (p1 (p2 x))))

;; (define my-cadr2 (my-compose car cdr))
;; (pp (my-cadr2 '(1 2 3 4)))

;; (define my-cddr2 (my-compose cdr cdr))
;; (pp (my-cddr2 '(1 2 3 4)))

;; Conditionals: if
(define (my-abs x)
  (if [< x 0] (- x) x))

;; (pp (my-abs 1))
;; (pp (my-abs 0))
;; (pp (my-abs -1))

;; Conditionals: and
(define (reciprocal2 x)
  (and (not (= x 0)) (/ 1 x)))

;; (pp (reciprocal2 2))
;; (pp (reciprocal2 0))
;; (pp (reciprocal2 -2))

;; Assertion violation
(define (reciprocal3 x)
  (if [and (number? x) (not (= x 0))]
      (/ 1 x)
      (error "reciprocal3: improper argument:" x)))

;; (pp (reciprocal3 2))
;; (pp (reciprocal3 0))
;; (pp (reciprocal3 -2))

;; Conditionals: cond
(define (my-sign x)
  (cond [(> x 0) 1]
        [(< x 0) -1]
        [else 0]))

;; (pp (my-sign 2))
;; (pp (my-sign 0))
;; (pp (my-sign -2))

(define (atom? x)
  (not (pair? x)))

;; (pp (atom? '(a . a)))
;; (pp (atom? '()))

(define (shorter x y)
  (if [<= (length x) (length y)] x y))

;; (pp (shorter '(a) '(b)))
;; (pp (shorter '(a) '(b c)))
;; (pp (shorter '(a b) '(c)))

;; Simple recursion
(define (my-length lst)
  (if [null? lst] 0 (+ (my-length (cdr lst)) 1)))

;; (pp (my-length '()))
;; (pp (my-length '(a)))
;; (pp (my-length '(a b)))

;; Trace procedure
;; (trace my-length)
;; (my-length '(a b c d))

;; Treat the structure of pairs as a list
;; Singly recursive step for cdr only!
(define (list-copy lst)
  (if [null? lst]
      '()
      (let ([fst (car lst)]
            [rst (cdr lst)])
        (cons fst (list-copy rst)))))

;; (pp (list-copy '()))
;; (pp (list-copy '(a b c)))

(define (membr x lst)
  (cond [(null? lst) #f]
        [(eqv? (car lst) x) lst]
        [else (membr x (cdr lst))]))

;; (pp (membr 'a '()))
;; (pp (membr 'a '(a b)))
;; (pp (membr 'a '(b a)))
;; (pp (membr 'a '(b c)))

(define (remv x lst)
  (cond [(null? lst) '()]
        [(eqv? (car lst) x) (remv x (cdr lst))]
        [else (cons (car lst) (remv x (cdr lst)))]))

;; (pp (remv 'a '()))
;; (pp (remv 'a '(b c)))
;; (pp (remv 'a '(a b c)))
;; (pp (remv 'a '(a b a c a a)))

;; Treat the structure of pairs as a tree
;; Doubly recursive step for both car and cdr!
(define (tree-copy tr)
  (if [not (pair? tr)]
      tr
      (cons (tree-copy (car tr)) (tree-copy (cdr tr)))))

;; (pp (tree-copy '()))
;; (pp (tree-copy '(a)))
;; (pp (tree-copy '(a b)))
;; (pp (tree-copy '(a b c)))
;; (pp (tree-copy '((a . b) . c)))

(define (my-map2 fun lst)
  (if [null? lst]
      '()
      (let ([fst (car lst)]
            [rst (cdr lst)])
        (cons (fun fst) (my-map2 fun rst)))))

;; (pp (my-map2 (lambda (x) (* x 10)) '()))
;; (pp (my-map2 (lambda (x) (* x 10)) '(1 2 3 4)))
;; (pp (map cons '(a b c) '(1 2 3)))

(define (my-append x y)
  (if [null? x] y (cons (car x) (my-append (cdr x) y))))

;; (pp (my-append '() '()))
;; (pp (my-append '(a) '()))
;; (pp (my-append '() '(A)))
;; (pp (my-append '(a) '(A)))
;; (trace my-append)
;; (pp (my-append '(a b) '(A B)))

(define (my-make-list n x)
  ;; (or (>= n 0) (error "my-make-list: negative argument" n))
  (and (< n 0) (error "my-make-list: negative argument" n))
  (if [= n 0] '() (cons x (my-make-list (- n 1) x))))

;; (pp (my-make-list 0 'a))
;; (pp (my-make-list 1 'a))
;; (pp (my-make-list 2 'a))
;; (pp (my-make-list 3 '()))
;; (pp (my-make-list -1 'a))

(define (my-list-ref lst i)
  (and (or (< i 0) (> i (- (length lst) 1)))
       (error "my-list-ref: index out of bounds" i))
  (if [= i 0] (car lst) (my-list-ref (cdr lst) (- i 1))))

;; (pp (my-list-ref '(a b c) 0))
;; (pp (my-list-ref '(a b c) 1))
;; (pp (my-list-ref '(a b c) 2))
;; (pp (my-list-ref '(a b c) 3))
;; (pp (my-list-ref '() 0))

(define (my-list-tail lst i)
  (and (or (< i 0) (> i (- (length lst) 1)))
       (error "my-list-tail: index out of bounds" i))
  (if [= i 0] lst (my-list-tail (cdr lst) (- i 1))))

;; (pp (my-list-tail '(a b c) 0))
;; (pp (my-list-tail '(a b c) 1))
;; (pp (my-list-tail '(a b c) 2))
;; (pp (my-list-tail '(a b c) 3))
;; (pp (my-list-tail '() 0))

(define (shorter? x y)
  (cond [(null? x) #t]
        [(null? y) #f]
        [else (shorter? (cdr x) (cdr y))]))

;; (pp (shorter? '() '()))
;; (pp (shorter? '(a) '()))
;; (pp (shorter? '() '(A)))
;; (pp (shorter? '(a b) '(A)))
;; (pp (shorter? '(a) '(A B)))

(define (shorter2 x y)
  (if [shorter? x y] x y))

;; (pp (shorter2 '() '()))
;; (pp (shorter2 '(a) '()))
;; (pp (shorter2 '() '(A)))
;; (pp (shorter2 '(a b) '(A)))
;; (pp (shorter2 '(a) '(A B)))

;; Mutual recursion: same base case, opposite outcomes
(define (my-even? x)
  (if [= x 0] #t (my-odd? (- x 1))))

(define (my-odd? x)
  (if [= x 0] #f (my-even? (- x 1))))

;; (trace my-even? my-odd?)
;; (pp (my-even? 7))
;; (pp (my-odd? 7))

(define (my-transpose lst)
  (let ([a (map car lst)]
        [b (map cdr lst)])
    (cons a b)))

;; (pp (my-transpose '((a . A) (b . B) (c . C))))

;; Top-level variable assignment
;; (define x 'a)
;; (set! x 'b)
;; (pp x)

;; let- and lambda-bound variable assignment
;; (pp (let ([x 'a])
;;   (set! x 'b)
;;   x))

;; Quadratic equition solver: ax^2 + bx + c = 0
(define* (solve-quadratic a #:optional (b 0) (c 0))
  (let* ([d (sqrt (- (expt b 2) (* 4 a c)))]
         [r1 (/ (+ (- b) d) (* 2 a))]
         [r2 (/ (- (- b) d) (* 2 a))])
    (cons r1 r2)))

;; (pp (solve-quadratic 1))
;; (pp (solve-quadratic 1 -2 -3))

;; Stack implementation using internal state and set! assignment
;; Abstract object design pattern
(define (make-stack)
  ;; State is maintained in each stack object closure
  (let ([stack '()])
    ;; Expose only set of function for object construction and manipulation
    ;; (public interface) and change internal implementaiton wihout impacting clients
    (lambda (message . args)
      (case message
        ('empty? (null? stack))
        ('push! (set! stack (cons (car args) stack)))
        ('top (car stack))
        ('pop! (set! stack (cdr stack)))
        ('ref (car (drop stack (car args))))
        ('set! (let* ([pos (car args)]
                      [val (cadr args)]
                      [head (take stack pos)]
                      [tail (set-car! (drop stack pos) val)])
                 (append head tail)))
        (else (error "stack: unsupported operation" message))))))

;; Each state reference or state change are made explicitly by the object (s1)
(define s1 (make-stack))
;; (pp (s1 'empty?))
;; (s1 'push! 'a)
;; (s1 'push! 'b)
;; (pp (s1 'empty?))
;; (pp (s1 'top))
;; (pp (s1 'ref 0))
;; (pp (s1 'ref 1))
;; (s1 'set! 0 'B)
;; (pp (s1 'ref 0))
;; (s1 'set! 1 'A)
;; (pp (s1 'ref 1))
;; (s1 'pop!)
;; (pp (s1 'top))
;; (s1 'pop!)
;; (pp (s1 'empty?))
;; (s1 'destroy)

;; Change list values via assignment
(define lst '(a b c d))
;; Change first
;; (set-car! lst 'A)
;; (pp lst)
;; Change second
;; (set-car! (cdr lst) 'B)
;; (pp lst)
;; (set-cdr! (cdr lst) '(C D))
;; (pp lst)

;; Queue implementation using list assignment operations
(define (make-queue)
  ;; Non-empty list (storage for the queue)
  (let ([end (cons 'ignored '())])
    ;; Header pair points to the start and the end of the list
    ;; Queue is accesed through the header
    (cons end end)))

(define (putq! queue value)
  (let ([end (cons 'ignored '())])
    (set-car! (cdr queue) value)
    (set-cdr! (cdr queue) end)
    (set-cdr! queue end)))

(define (getq queue)
  (if [emptyq? queue] (error "getq: empty queue"))
  (car (car queue)))

(define (delq! queue)
  (if [emptyq? queue] (error "delq!: empty queue"))
  (set-car! queue (cdr (car queue))))

(define (emptyq? queue)
  (equal? (car queue) (cdr queue)))

;; (trace make-queue)
;; (trace putq!)
;; (trace delq!)
;; (define q1 (make-queue))
;; (pp (emptyq? q1))
;; (putq! q1 'a)
;; (pp (emptyq? q1))
;; ;; (pp (getq q1))
;; (putq! q1 'b)
;; (putq! q1 'c)
;; (putq! q1 'd)
;; (pp (getq q1))
;; (delq! q1)
;; (pp (getq q1))
;; (delq! q1)
;; (pp (getq q1))
;; (delq! q1)
;; (delq! q1)
;; (pp (emptyq? q1))
;; (pp (delq! q1))

;; letrec for definition of recursive procedures
;; (pp (letrec ([sum (lambda (lst)
;;                     (if (null? lst) 0 (+ (car lst) (sum (cdr lst)))))])
;;       (sum '(1 2 3 4 5))))

;; letrec for definition of mutually recursive procedures
;; (pp (letrec ([even? (lambda (x)
;;                       (if (= x 0) #t (odd? (- x 1))))]
;;              [odd? (lambda (x)
;;                      (if (= x 0) #f (even? (- x 1))))])
;;       (list (even? 14) (odd? 14))))

;; Variables depend on each other
;; (pp (let* ([x 1]
;;              [y (+ x 1)])
;;       (+ x y)))

;; Factorial with named (let: single recursive
(define (factorial n)
  (let fact ([i n] [res 1])
    (if [= i 0] res (fact (- i 1) (* i res)))))

;; (pp (factorial 5))

;; Fibonacci numbers with named (let: double recursive
(define (fibonacci n)
  (let fib ([i 0] [curr 1] [prev 0])
    (if [= i n] curr (fib (+ i 1) (+ curr prev) curr))))

;; (pp (fibonacci 0))
;; (pp (fibonacci 1))
;; (pp (fibonacci 10))

(define (factor n)
  ;; Initialization: n - argument, i - current factor, fs - factors accumulator
  (let ftor ([n n] [i 2] [fs '()])
    ;; Edge case for 0 and 1
    (cond [(and (<= 0 n 1) (null? fs)) (cons n fs)]
          ;; Base case: ran out of factors
          [(> i n) fs]
          ;; Recursive case: factor found
          [(integer? (/ n i)) (ftor (/ n i) i (cons i fs))]
          ;; Recursive case: check next factor
          [else (ftor n (+ i 1) fs)])))

;; (pp (factor 0))
;; (pp (factor 1))
;; (pp (factor 2))
;; (pp (factor 3))
;; (pp (factor 4))
;; (pp (factor 5))
;; (pp (factor 6))
;; (pp (factor 7))
;; (pp (factor 8))
;; (pp (factor 9))
;; (pp (factor 10))
;; (pp (factor 12))
;; (pp (factor 18))

;; Continuations

;; Continuation k is never used
;; Value is the product: 20
;; (pp (call/cc (lambda (k) (* 5 4))))

;; Continuation k is invoked before the multiplication
;; Value is the value passed to the continuation: 4
;; (pp (call/cc (lambda (k) (* 5 (k 4)))))

;; Confituation k includes the addition by 2
;; Value is the value passed to the confituaiton plus 2: 6
;; (pp (+ 2 (call/cc (lambda (k) (* 5 (k 4))))))

;; Continuation may provide a non-local exit from a recustions
(define (product . lst)
  ;; Capture continuation
  (call/cc
   (lambda (break)
     (let prod ([lst lst] [prd 1])
       (cond [(null? lst) prd]
             ;; Return immediately by invoking the break continuation
             ;; Break continuation returns the value to the point
             ;; where continuation was captured
             [(= (car lst) 0) (break 0)]
             [else (prod (cdr lst) (* (car lst) prd))])))))

;; (pp (product))
;; (pp (product 0))
;; (pp (product 1))
;; (pp (product 1 2))
;; (pp (product 1 2 0 3))

;; CPS: Continuation Passing Style
(define (div x y success failure)
  (if (= y 0)
      (failure "division by zero")
      (success (quotient x y) (remainder x y))))

;; (pp (div 5 3 list identity)); single identity
;; (pp (div 1 0 list values)); generalized identity
