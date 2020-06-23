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

(define (remove-first x l)
  (cond
    [(null? l) '()]
    [else (cond
            [(eqv? x (car l)) (cdr l)]
            [else (remove-first x (cdr l))])]))

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

(pp (string-interleave-camel-case "vlad and lana" #f))
