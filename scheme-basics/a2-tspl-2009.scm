;; Chapters 6-

(use-modules (ice-9 pretty-print))

(define pp pretty-print)

;; (quote = '(... (quasiquote = `(... (unquote = ,... (unquote-splicing = ,@...
;; (pp `(+ 2 ,(* 3 4)))
;; (pp `(+ ,(cdr '(* 2 3))))
;; (pp `(+ ,@(cdr '(* 2 3))))

;; Eqivalence predicates
;; (pp (eq? 'a 'a))
;; (pp (eq? 1 1))
;; (pp (eq? "Vlad" "Vlad"))

;; (pp (= -0.0 0.0))
;; (pp (eqv? -0.0 0.0))
;; (pp (= 3.0 3.0+0.0i))
;; (pp (eqv? 3.0 3.0+0.0i))

;; (pp (equal? '(1 2 3) '(1 2 3)))

(define (my-list-ref lst n)
  (if [zero? n] (car lst) (my-list-ref (cdr lst) (- n 1))))

;; (pp (my-list-ref 'a 0))
;; (pp (my-list-ref '() 0))
;; (pp (my-list-ref '() 1))
;; (pp (my-list-ref '(1 2 3) 0))
;; (pp (my-list-ref '(1 2 3) 2))

(define (my-list-ref2 lst n)
  (do ([n n (- n 1)] [lst lst (cdr lst)])
      ([zero? n] (car lst))))

;; (pp (my-list-ref2 'a 0))
;; (pp (my-list-ref2 '() 0))
;; (pp (my-list-ref2 '() 1))
;; (pp (my-list-ref2 '(1 2 3) 0))
;; (pp (my-list-ref2 '(1 2 3) 2))

(define (my-list-tail lst n)
  (if [zero? n] lst (my-list-tail (cdr lst) (- n 1))))

;; (pp (my-list-tail '(1 2 3 4) 0))
;; (pp (my-list-tail '(1 2 3 4) 2))

(define (my-list-tail2 lst n)
  (do ([n n (- n 1)] [lst lst (cdr lst)])
      ([zero? n] lst)))

;; (pp (my-list-tail2 '(1 2 3 4) 0))
;; (pp (my-list-tail2 '(1 2 3 4) 2))

(define (my-append . args)
  (let append-lists ([res '()] [args args])
    (if [null? args]
        ;; All input lists are appended
        res
        ;; Append current list
        (let append-list ([lst res])
          (if [null? lst]
              ;; Consume next input list
              (append-lists (car args) (cdr args))
              ;; Consume current list
              (cons (car lst) (append-list (cdr lst))))))))

;; (pp (my-append '(a) '(b) '(c)))
;; (pp (my-append '(a a) '(b b) '(c c)))

(define (my-reverse lst)
  (let rev ([lst lst] [res '()])
    (if [null? lst] res (rev (cdr lst) (cons (car lst) res)))))

;; (pp (my-reverse '(1 2 3 4)))

(define (my-member x lst)
  (cond
    [(null? lst) #f]
    [(equal? x (car lst)) lst]
    [else (my-member x (cdr lst))]))

;; (pp (my-member 'b '(a b c)))
;; (pp (my-member 'd '(a b c)))

(define (my-member? x lst)
  (and (my-member x lst) #t))

;; (pp (my-member? 'b '(a b c)))
;; (pp (my-member? 'd '(a b c)))

(define (count-occurences x lst)
  (let count-occ ([cnt 0] [lst lst])
    (cond
      [(null? lst) cnt]
      [(equal? (car lst) x) (count-occ (+ cnt 1) (cdr lst))]
      [else (count-occ cnt (cdr lst))])))

;; (pp (count-occurences 'a '(a b a c a d)))
;; (pp (count-occurences 'e '(a b a c a d)))

(define (count-occurences2 x lst)
  (do ([cnt 0 (if [equal? (car lst) x] (+ cnt 1) cnt)] [lst lst (cdr lst)])
      ([null? lst] cnt)))

;; (pp (count-occurences2 'a '(a b a c a d)))
;; (pp (count-occurences2 'e '(a b a c a d)))

(define (my-memp p lst)
  (cond
    [(null? lst) #f]
    [(p (car lst)) lst]
    [else (my-memp p (cdr lst))]))

;; (pp (my-memp my-even? '(1 2 3 4 5)))
;; (pp (my-memp my-odd? '(1 2 3 4 5)))
;; (pp (my-memp symbol? '(1 2 3 4 5)))

(define (my-remp p lst)
  (let rem ([lst lst] [res '()])
    (cond
      [(null? lst) (my-reverse res)]
      [(p (car lst)) (rem (cdr lst) res)]
      [else (rem (cdr lst) (cons (car lst) res))])))

;; (pp (my-remp odd? '(1 2 3 4 5)))

(define (my-filter2 p lst)
  (let fil ([lst lst] [res '()])
    (cond
      [(null? lst) (my-reverse res)]
      [(p (car lst)) (fil (cdr lst) (cons (car lst) res))]
      [else (fil (cdr lst) res)])))

;; (pp (my-filter2 odd? '(1 2 3 4 5)))

(define (my-partition p lst)
  (let part ([lst lst] [tl '()] [fl '()])
    (cond
      [(null? lst) (values (my-reverse tl) (my-reverse fl))]
      [(p (car lst)) (part (cdr lst) (cons (car lst) tl) fl)]
      [else (part (cdr lst) tl (cons (car lst) fl))])))

;; (pp (call-with-values (lambda () (my-partition odd? '(1 2 3 4 5 6 7 8))) list))
