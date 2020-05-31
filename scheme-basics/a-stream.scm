(define-module (a-stream)
  #:export (my-delay my-force stream-counter stream-car stream-cdr stream-add))

(define (make-promise p)
  (let ([val #f] [set? #f])
    (lambda ()
      (unless set?
        (let ([v (p)])
          ;; Protection against forcing promise recursively
          (unless set?
            (set! val (p))
            (set! set? #t))))
      val)))

(define-syntax my-delay
  (syntax-rules ()
    [(_ expr) (make-promise (lambda () expr))]))

(define (my-force p)
  (p))

(define (stream-counter)
  (let next ([n 1])
    (my-delay (cons n (next (+ n 1))))))

(define (stream-car s)
  (car (my-force s)))

(define (stream-cdr s)
  (cdr (my-force s)))

(define (stream-add s1 s2)
  (my-delay (cons
             (+ (stream-car s1) (stream-car s2))
             (stream-add (stream-cdr s1) (stream-cdr s2)))))
