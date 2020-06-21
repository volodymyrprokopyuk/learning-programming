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
