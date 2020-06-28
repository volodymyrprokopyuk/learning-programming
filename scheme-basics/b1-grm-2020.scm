(use-modules (ice-9 pretty-print))

(define pp pretty-print)

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (lat? l)
  (cond
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]))

;; (pp (lat? '(a b c d)))
;; (pp (lat? '(a b c d ())))

(define (member? x l)
  (cond
    [(null? l) #f]
    [(eqv? x (car l)) l]
    [else (member? x (cdr l))]))

;; (pp (member? 'b '(a b c)))
;; (pp (member? 'd '(a b c)))

(define (remove-all x l)
  (reverse
   (let remove-all' ([l l] [r '()])
     (cond
       [(null? l) r]
       [(eqv? x (car l)) (remove-all' (cdr l) r)]
       [else (remove-all' (cdr l) (cons (car l) r))]))))

;; (pp (remove-all 'b '(a b c b d)))
;; (pp (remove-all 'd '(a b c)))

(define (remove-first x l)
  (reverse
   (let remove-first' ([l l] [r '()])
     (cond
       [(null? l) r]
       [(eqv? x (car l)) (append (reverse (cdr l)) r)]
       [else (remove-first' (cdr l) (cons (car l) r))]))))

;; (pp (remove-first 'b '(a b c b d)))
;; (pp (remove-first 'd '(a b c)))

(define (my-last l)
  (cond
    [(null? l) (error "last: empty list")]
    [(null? (cdr l)) (car l)]
    [else (my-last (cdr l))]))

;; (pp (my-last '(a)))
;; (pp (my-last '(a b c)))
;; (pp (my-last '()))

(define* (make-counter #:optional (start 0) (step 1))
  (let ([c start])
    (lambda ()
      (set! c (+ c step))
      c)))

;; (let ([c1 (make-counter)]
;;       [c2 (make-counter 100 10)])
;;   (pp (c1))
;;   (pp (c2))
;;   (pp (c1))
;;   (pp (c1))
;;   (pp (c1))
;;   (pp (c2)))

(use-modules (srfi srfi-11)) ; let-values

(define (make-account)
  (let* ([balance 0]
         [get-balance (lambda () balance)]
         [deposit (lambda (amount) (set! balance (+ balance amount)) balance)])
    (values get-balance deposit)))

;; (let-values ([(get-balance deposit) (make-account)])
;;   (pp (get-balance))
;;   (deposit 50)
;;   (pp (get-balance))
;;   (deposit -40)
;;   (pp (get-balance)))

(define (make-account2)
  (let ([balance 0])
    (define (get-balance) balance)
    (define (deposit amount) (set! balance (+ balance amount)) balance)
    (lambda args
      (apply
       (case (car args)
         [(get-balance) get-balance]
         [(deposit) deposit]
         [else (error "account: invalid method")])
       (cdr args)))))

;; (let ([account (make-account2)])
;;   (pp (account 'get-balance))
;;   (account 'deposit 50)
;;   (pp (account 'get-balance))
;;   (account 'deposit -40)
;;   (pp (account 'get-balance))
;;   (account 'unknown-method))

;; MY_VAR=Vlad guile3.0 -c '(display (getenv "MY_VAR"))'
;; guile3.0 -c '(display (command-line))' a b c d
;; (pp (command-line))

;; guile3.0 -e print-last-argument b1-tls-1996.scm a b c d
(define (print-last-argument args)
  (pp (my-last args)))

(define* (string-interleave-camel-case s #:optional (upcase? #t))
  (let* ([up? upcase?]
         [f (lambda (c)
              (if up?
                  (begin (set! up? #f) (char-upcase c))
                  (begin (set! up? #t) (char-downcase c))))])
    (string-map f s)))

;; (pp (string-interleave-camel-case "vlad and lana" #f))

;; Keyword arguments with default values
(define* (opt-args #:key (a 1) (b 2))
  (list a b))

;; (pp (opt-args))
;; (pp (opt-args #:a 10))
;; (pp (opt-args #:b 20))
;; (pp (opt-args #:b 200 #:a 100))

(use-modules (srfi srfi-9) ; define-record-type
             (srfi srfi-9 gnu)) ; set-record-type-printer!

(define-record-type employee
  ;; Constructor
  (make-employee name salary)
  ;; Type predicate
  employee?
  ;; Accessor only (immutable field)
  (name employee-name)
  ;; Accessor + mutator
  (salary employee-salary set-employee-salary!))

;; (pp (let ([vlad (make-employee "Vlad" 5000)])
;;       (pp vlad)
;;       (set-employee-salary! vlad 10000)
;;       vlad))

;; Custom record printer
(set-record-type-printer!
 employee
 (lambda (record port)
   (write-char #\[ port)
   (display (employee-name record) port)
   (write-char #\] port)))

;; (pp (let ([vlad (make-employee "Vlad" 5000)])
;;       vlad))

;; Asociation list
;; (pp (let* ([al '((a . 1) (b . 2) (c . 3))]
;;            [al2 (acons 'z 0 al)]
;;            [al3 (assq-set! al2 'z 100)])
;;       (list (assq 'z al3) (assq-ref al3 'z)
;;             (assq-remove! al3 'z))))

;; Hash table
;; (let ([h (make-hash-table)])
;;   (hashq-set! h 'a 1)
;;   (hashq-set! h 'b 2)
;;   (hashq-set! h 'c 3)
;;   (pp (hashq-ref h 'a))
;;   (pp (hashq-get-handle h 'b))
;;   (pp (hash-fold (lambda (k v b) (+ b v)) 0 h))
;;   (pp (hash-fold (lambda (k v b) (1+ b)) 0 h)))

;; case-lambda
(define* (make-accum #:optional (a 0))
  (case-lambda
    [() a]
    [(b) (set! a (+ a b)) a]))

;; (let ([acc (make-accum)])
;;   (pp (acc))
;;   (acc 10)
;;   (acc 20)
;;   (pp (acc)))

(define plus
  (case-lambda
    "Returns sum of all arguments"
    [() 0]
    [(a) a]
    [(a b) (+ a b)]
    [(a b . r) (apply plus (+ a b) r)]))

;; (pp (procedure-documentation plus))
;; (pp (plus 1 2 3 4 5))

(use-modules (srfi srfi-1)) ; fold

(define (my-length l)
  "Returns length of a list"
  (fold + 0 (map (const 1) l)))

;; (pp (procedure-documentation my-length))
;; (pp (my-length '(1 2 3 4)))

;; Procedure with setter
(define vec (make-procedure-with-setter vector-ref vector-set!))

;; (let ([v (vector 'a 'b 'c)])
;;   ;; (pp (vector-ref v 1))
;;   ;; (vector-set! v 1 'B)
;;   ;; (pp (vector-ref v 1)))
;;   (pp (vec v 1))
;;   (set! (vec v 1) 'B)
;;   (pp (vec v 1)))

;; Global macro
(define-syntax my-when
  (syntax-rules ()
    [(_ condition expression ...)
     (if condition (begin expression ...))]))

;; (my-when #t
;;          (pp "Vlad")
;;          (pp "Lana"))

;; Local macro
;; (let-syntax ([my-unless
;;                  (syntax-rules ()
;;                      [(_ condition expression ...)
;;                       (if (not condition) (begin expression ...))])])
;;   (my-unless #f
;;     (pp "Vlad")
;;     (pp "Lana")))

;; (pp (sort '(4 3 6 7 9 2 8 7) <))

;; Hooks
;; (let ([h (make-hook 1)])
;;   (add-hook! h (lambda (x) (pp (string-append "Hook A: " x))))
;;   (add-hook! h (lambda (x) (pp (string-append "Hook B: " x))))
;;   (run-hook h "ok"))

;; Multiple return values. Formals binding as in (lambda
;; (define-values (a b) (values 1 2))
;; (pp (cons a b))
;; (define-values (a . b) (values 1 2 3 4))
;; (pp (list a b))
;; (define-values a (values 1 2 3 4))
;; (pp a)
