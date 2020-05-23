(use-modules (ice-9 pretty-print))

(define pp pretty-print)

;; Syntactic extension with (define-syntax
;; (define-syntax associates transformation procedure with a keyword
(define-syntax my-let ; keyword
  ;; Transformation procedure (transformer)
  (syntax-rules () ;
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

;; (pp (my-let ((x 'vlad) (y 'lana)) (cons x y)))

;; Recursive definition
(define-syntax my-and
  (syntax-rules ()
    ;; No arguments
    [(_) #t]
    ;; Base case
    [(_ e) e]
    ;; Recursion step translates (and into nested (if expressions
    [(_ e1 e2 e3 ...)
     (if e1 (and e2 e3 ...) #f)]))

;; (pp (my-and))
;; (pp (my-and #f))
;; (pp (my-and #t 'a))
;; (pp (my-and 'a 'b #f))

;; Recursive definition with temporary variable
(define-syntax my-or
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     ;; Automatic renaming of introduced identifiers
     (let ([t e1])
       (if t t (or e2 e3 ...)))]))

;; (pp (my-or))
;; (pp (my-or #f))
;; (pp (my-or #f 'a))
;; (pp (my-or 'a 'b))

;; (when and (unless for side effects instead of one-armed (if
;; (let ([x #f])
;;   (when x (pp 'true))
;;   (unless x (pp 'false)))
