(define-module (a-stream)
  ;; #:use-module (srfi srfi-9)
  #:export (my-delay my-force stream-counter stream-car stream-cdr stream-add))

;; (define-record-type myp
;;   (make-myp promise vals set?)
;;   myp?
;;   (promise myp-promise)
;;   (vals myp-vals set-myp-vals!)
;;   (set? myp-set? set-myp-set!))

;; (make-promise with single return value
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
  ;; (unless (myp? p)
  ;;   (error "my-force: myp promise expected" p))
  (p))

(define (stream-counter)
  (let next ([n 1])
    ;; Lazy, recursive, infinite list
    (my-delay (cons n (next (+ n 1))))))

(define (stream-car s)
  ;; Evaluate one value at a time
  (car (my-force s)))

(define (stream-cdr s)
  ;; Evaluate one value at a time
  (cdr (my-force s)))

(define (stream-add s1 s2)
  ;; Recursively delay computation and construction of each compound list element
  (my-delay (cons
             (+ (stream-car s1) (stream-car s2))
             (stream-add (stream-cdr s1) (stream-cdr s2)))))
