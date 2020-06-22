#!/usr/bin/guile3.0 \
-e main -s
!#

;; ./b-script.scm a b c
;; ./b-script.scm 5 => 120

(define (factorial n)
  (let factorial' ([n n] [r 1])
    (if (zero? n) r (factorial' (- n 1) (* r n)))))

(define (main args)
  ;; (map (lambda (arg) (display arg) (newline)) args))
  (let* ([n (string->number (cadr args))]
         [fact (factorial n)])
    (display fact) (newline)))
