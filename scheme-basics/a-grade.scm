(define-module (a-grade)
  #:export (gpa gpa->grade))

(define (letter->number l)
  (case l
    [(a) 4.0]
    [(b) 3.0]
    [(c) 2.0]
    [(d) 1.0]
    [else (error "letter->number: invalid grade letter:" l)]))

(define-syntax gpa
  (syntax-rules ()
    [(_ g1 g2 ...)
     (let ([gs (map letter->number '(g1 g2 ...))])
       (/ (apply + gs) (length gs)))]))

(define (in-range? l x u)
  (and (>= x l) (< x u)))

(define-syntax range-case
  (syntax-rules (- else)
    [(_ expr
        [(x - y) e1 e2 ...]
        ...
        [else ee1 ee2 ...])
     (cond
       [(in-range? x expr y) e1 e2 ...]
       ...
       [else ee1 ee2 ...])]))

(define (gpa->grade gp)
  (range-case gp
    [(0.0 - 0.5) 'f]
    [(0.5 - 1.5) 'd]
    [(1.5 - 2.5) 'd]
    [(2.5 - 3.5) 'b]
    [else 'a]))
