;; #lang racket
;; (require racket)
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
;; (struct student (name id# dorm) #:transparent) ; transparent structure

;; (define vlad (student 'Vlad 1 'dorm1))
;; (define lana (student 'Lana 2 'dorm2))
;; (student-name vlad)
;; (student-id# vlad)
;; (student-dorm vlad)
;; (student? vlad)

;; (define students (list vlad lana))
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

;; (struct point (x y))

;; (define (distance-to-origin p)
;;   (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))

;; (define p1 (point 3 4))
;; (distance-to-origin p1)

;; equal? -> value equality
;; Recursive definition of equal? for every data type (dynamically added)
;; (define (my-equal? a b)
;;   (cond [(and (point? a) (point? b)) ; type check
;;          (and (my-equal? (point-x a) (point-x b)) ; recursive value check
;;               (my-equal? (point-y a) (point-y b)))]
;;         [(and (number? a) (number? b))
;;          (= a b)]
;;         [else #f]))

;; (define p2 (point 1 2))
;; (define p3 (point 3 4))
;; (my-equal? p1 p3)

;; Alias for a variable
;; (define p4 p1)
;; eq? instance equality
;; (eq? p1 p3) ; #f
;; (eq? p1 p4) ; #t

;; (error 'main "Oh")

;; Local definitions
(define (my-max lst dft)
  (cond [(empty? lst) (list dft)]
        [else
         (let ([fst (first lst)]
               [rst (rest lst)])
           (if (> fst dft) (my-max rst fst) (my-max rst dft)))]))

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
;; (struct parent ([knowledge #:mutable]) #:transparent) ; per field mutability
;; (struct child parent (health) #:mutable #:transparent) ; whole structure mutable

;; (define a-child (child 'a-knowledge 'a-health))
;; a-child
;; (parent-knowledge a-child)
;; (child-health a-child)
;; Mutate structures
;; (set-parent-knowledge! a-child 'new-knowldege)
;; (set-child-health! a-child 'new-health)
;; a-child

;; (define (update-struct! mutator)
;;   (lambda (str val)
;;     (mutator str val)))

;; (define parent-knowledge!
;;   (update-struct! set-parent-knowledge!))

;; (define child-health!
;;   (update-struct! set-child-health!))

;; (parent-knowledge! a-child 'updated-knowledge)
;; (child-health! a-child 'updated-health)
;; a-child

;; (let sets up local definitions)
;; (equal?
;;  (let ((ch (child 'knowledge 'health)))
;;    (set-child-health! ch 'mutated-health)
;;    (child-health ch))
;;  'mutated-health)

;; case statement
(define (classify num)
  (case num
      [(1 2 3) 'small]
      [(4 5 6) 'big]
      [else 'very-big]))

;; (classify 2)
;; (classify 5)
;; (classify 15)

;; For side effects only
;; (for ([i '(1 2 3 4)])
;;   (display i))

;; (for-each display '(1 2 3 4))

;; Like map
;; (for/list ([i '(1 2 3 4)])
;;   (/ 1 i))

;; (map (lambda (i) (/ 1 i)) '(1 2 3 4))

;; Like foldr
;; (for/fold ([int 0])
;;           ([i '(1 2 3 4)])
;;   (+ int i))

;; (foldr (lambda (i int) (+ int i)) 0 '(1 2 3 4))

;; Keep state and return multiple values
;; (for/fold ([int 0]
;;            [cnt 0])
;;           ([i '(1 2 3 4)])
;;   ;; return multiple values at once
;;   (values (+ int i)
;;           (add1 cnt)))

;; (define-values (one two three) (values 'one 'two 'three))
;; (list one two three)

;; (for ([i '(1 2 3 4 5 6 7)] #:when (odd? i))
;;      (display i))

;; (for/fold ([sum 0])
;;           ([i '(1 2 3 4 5 6 7)] #:when (even? i))
;;   (+ sum i))

;; (for/list ([i '(1 2 3 4 5 6)]
;;            [j '(10, 20, 30, 40, 50)]
;;            [k '(100, 200, 300, 400)])
;;   (list i j k))

;; (for* ([i '(1 2 3)]
;;        [j '(one two three)]
;;        [k '(a b c)])
;;   (display (list i j k)))

;; (for*/list ([i '((1 2) (3 4) (5 6))]
;;             [j i])
;;   j)

;; (for/list ([i (in-range 10 20 2)])
;;   i)

;; Lazy evaluation with lambda
(define lazy+ (lambda () (apply + '(1 2 3 4 5))))
;; (lazy+)

;; Memoization is result caching via closure
(define (memoize fun)
  (define run? #f)
  (define memoized (void))
  (lambda ()
    (cond [run? memoized]
          [else (set! run? #t) (set! memoized (fun)) memoized])))

(define (big-computation)
  (println 'big-computation)
  'result)

(define mbig-computation (memoize big-computation))
;; (mbig-computation)
;; (mbig-computation)

(define (memoize2 fun)
  (define (cache)
    (define memoized (fun))
    (set! cache (lambda () memoized))
    memoized)
  (lambda () (cache)))

(define m2big-computation (memoize big-computation))
;; (m2big-computation)
;; (m2big-computation)

;; Optional rest parameters
;; ((lambda (a b . rst)
;;    (list a b rst)) 'a 'b 'c 'd 'e)

;; ((lambda (a b #!rest c)
;;    (list a b c)) 'a 'b 'c 'd)

;; Optional unnamed parameters
;; ((lambda (a b #!optional c (d 1) f)
;;    (list a b c d f)) 'a 'b 'c 'd 'f)

;; Optional named parameters
;; ((lambda (a b #!key c (d 1))
;;    (list a b c d)) 'a 'b d: 'd c: 'c)

;; First-class functions
;; ((lambda (a b)
;;    (for-each (lambda (op) (println (op a b))) (list + - * /))) 2 3)

;; Recursion
;; (let loop ((i 5))
;;   (println i)
;;   (if (> i 0) (loop (- i 1))))

;; (let loop ((i 0))
;;   (println i)
;;   (if (< i 5) (loop (+ i 1))))

;; Evaluation of s-expressions
;; (eval '(display 'ok))
;; (eval '(let ((a 'ok)) (println a)))

;; Closures
(define (counter)
  (let ((cnt 0))
    (lambda ()
      (set! cnt (+ cnt 1))
      cnt)))

(define counter1 (counter))
(define counter2 (counter))
;; (counter1)
;; (counter2)
;; (counter1)
;; (counter1)
;; (counter2)

#|
Block comment
|#

(define square2 (lambda (x) (* x x)))

(define reciprocal (lambda (x) (if (= x 0) "oh" (/ 1 x))))

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
;; (my-list 1 2 3 4)

(define my-cadr (lambda (lst) (car (cdr lst))))
;; (my-cadr '(1 2 3 4))

(define my-cddr (lambda (lst) (cdr (cdr lst))))
;; (my-cddr '(1 2 3 4))

;; Top-level lambda definition (abbrieviated)
(define (my-list2 . rst)
  rst)
;; (my-list2)
;; (my-list2 1)
;; (my-list2 1 2)

(define (doubler fun)
  (lambda (x) (fun x x)))
(define double+ (doubler +))
;; (double+ 1/2)

;; Procedure can be defined in any order
;; Reference proc2, then define proc2
(define (proc1) (proc2))
(define (proc2) 'proc2-from-proc1)
;; (proc1)

;; Procedure composition (less efficient)
(define (my-compose p1 p2)
  (lambda (x) (p1 (p2 x))))
(define my-cadr2 (my-compose car cdr))
;; (pp (my-cadr2 '(1 2 3 4)))
(define my-cddr2 (my-compose cdr cdr))
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

;; Simple recutsion
(define (my-length lst)
  (if [null? lst] 0 (+ (my-length (cdr lst)) 1)))
;; (pp (my-length '()))
;; (pp (my-length '(a)))
;; (pp (my-length '(a b)))

;; Trace procedure
;; (trace my-length)
;; (my-length '(a b c d))
