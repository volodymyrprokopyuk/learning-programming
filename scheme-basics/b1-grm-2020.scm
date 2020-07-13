(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

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

(use-modules (srfi srfi-11)) ;; let-values

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

(use-modules (srfi srfi-9) ;; define-record-type
             (srfi srfi-9 gnu)) ;; set-record-type-printer!

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

(use-modules (srfi srfi-1)) ;; fold

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

;; Iteration with (do
;; (pp (do ([i 0 (1+ i)]
;;          [j 0 (+ j 10)])
;;         ((> i 5) 'done)
;;       (format #t "~s ~s\n" i j)))

;; Iteration with named (let
;; (pp (let loop ([i 0] [j 0])
;;       (format #t "~s ~s\n" i j)
;;       (if (< i 5) (loop (1+ i) (+ j 10)) 'done)))

;; Prompt/abort
(define prompt-cont
  (call-with-prompt
   ;; Tag
   'ptag
   ;; Thunk
   (lambda () (+ 10 (abort-to-prompt 'ptag)))
   ;; Handler
   (lambda (k) k)))

;; (pp (prompt-cont 1))
;; Composable continuation
;; (pp (* 2 (prompt-cont 1)))

(use-modules (ice-9 control)) ;; call/ec

;; Escape continuation
(define (my-prefix x l)
  (call/ec
   (lambda (return-ec)
     (fold
      (lambda (el res)
        (if (eqv? el x)
            (return-ec (reverse res)) ;; escape fold
            (cons el res)))
      '() l))))

;; (pp (my-prefix 'd '(a b c d e f g)))

;; Multiple values
;; (pp (call-with-values
;;      (lambda () (values 1 2))
;;      (lambda (a b) (+ a b))))

(use-modules (ice-9 receive)) ;; receive

;; Receive multiple values
;; (pp (receive (a b)
;;         ((lambda () (values 1 2)))
;;       (+ a b)))

;; (use-modules (ice-9 exceptions)) ;; with-exception-hanlder

;; Exceptions
;; (with-exception-handler
;;   ;; Handler
;;   (lambda (exc) (pp exc))
;;   ;; Thunk
;;   (lambda () (raise-exception 'oh #:continuable? #t)))

;; Resources management
;; (call/cc
;;  (lambda (escape)
;;    (dynamic-wind
;;      ;; In-guard
;;      (lambda () (pp 'in-guard))
;;      ;; Thunk
;;      (lambda () (pp 'thunk) (escape) #;(raise-exception 'oh))
;;      ;; Out-guard
;;      (lambda () (pp 'out-guard)))))

;; Parameters (dynamic variable binding)
;; (let* ([my-param (make-parameter 'a)]
;;        [show-param (lambda () (pp (my-param)))])
;;   (pp (my-param))
;;   (my-param 'b)
;;   (pp (my-param))
;;   (parameterize ((my-param 'c))
;;     (show-param)))

;; Ports
;; (let ([p (open-output-file "file.txt")])
;;   (display "Hello Vlad and Lana!" p)
;;   (close-port p))

;; (call-with-output-file "file2.txt"
;;   (lambda (p)
;;     (display "Vlad and Lana" p)
;;     (display (format #f "\nLanguage: ~A, implementation: ~A" "Scheme" "Guile") p)))

(use-modules (ice-9 textual-ports)) ;; get-string-all

;; (display (call-with-input-file "b1-grm-2020.scm" get-string-all))

;; (with-input-from-file "b1-grm-2020.scm"
;;   (lambda () (display (get-string-all (current-input-port)))))

;; (with-output-to-file "file.txt"
;;   (lambda () (display "Vlad and Lana!" (current-output-port))))

(use-modules (ice-9 regex)) ;; string-match

;; (pp (string-match "V.{3}" "Vlad and Lana"))

;; (pp (let ([r (make-regexp "([a-z]+)" regexp/icase)]
;;           [s "Vlad and Lana1"])
;;       (regexp-exec r s)
;;       (map match:substring (list-matches r s))
;;       (fold-matches r s '() (lambda (m b) (cons (match:substring m) b)))))

;; (pp (regexp-substitute #f (string-match "[0-9]+" "begin 34 end") 'pre "45" 'post))

;; (pp (let ([r (make-regexp "([0-9]{4})([0-9]{2})([0-9]{2})")]
;;           [s "Date 20200702"])
;;       (regexp-substitute #f (regexp-exec r s) 'pre 1 "-" 2 "-" 3)))

;; (pp (let ([r (make-regexp " +")]
;;           [s "out of  the   box"])
;;       (regexp-substitute/global #f r s 'pre "-" 'post)))

;; (pp (let ([r (make-regexp "(.)(.)")]
;;           [s "Vlad"])
;;       (regexp-substitute/global #f r s 2 1 'post)))

;; Promise
;; (pp (let ([p (delay (+ 1 2))])
;;       (force p)))

;; SRFI-1 List library
(use-modules
 (srfi srfi-1)
 (srfi srfi-11) ;; let-values
 (ice-9 receive))

(let ([p (cons 1 2)]
      [p2 (cons 1 2)]
      [l (list 1 2 3 4)]
      [l2 (list 1 2 3 4)])
  (pp p)
  (pp l)
  (pp (eq? p p2))
  (pp (eqv? p p2))
  (pp (equal? p p2))
  (pp (list-tabulate 5 (lambda (e) (* e 10))))
  (pp (iota 5 0 10))
  (pp (list= eq? l l2))
  (pp (list-ref l 3))
  (pp (take l 3))
  (pp (drop l 1))
  (pp (take-right l 2))
  (pp (drop-right l 2))
  (let-values ([(a b) (split-at l 1)])
    (pp a) (pp b))
  (receive (a b) (split-at l 3)
    (pp a) (pp b))
  (pp (last l))
  (pp (last-pair l))
  (pp (length l))
  (pp (append l l2))
  (pp (concatenate (list l l2)))
  (pp (reverse l))
  (pp (append-reverse l '()))
  (pp (apply map list (list l l2)))
  (pp (zip l l2))
  (pp (zip l))
  (let ([ll (zip l)]
        [ll2 (zip l l2)])
    (pp (unzip1 ll))
    (receive [a b] (unzip2 ll2)
      (pp a) (pp b)))
  (pp (count even? l))
  (pp (fold + 0 l))
  (pp (fold + 0 (map (const 1) l)))
  (pp (fold cons '() l))
  (pp (fold-right cons '() l))
  (pp (unfold (lambda (e) (> e 10)) (lambda (e) (expt e 2)) (lambda (s) (1+ s)) 1))
  (pp (unfold null? car cdr l))
  (pp (unfold-right zero? (lambda (e) (expt e 2)) (lambda (s) (1- s)) 10))
  (pp (unfold-right null? car cdr l))
  (pp (filter odd? l))
  (pp (remove odd? l))
  (let-values ([(a b) (partition odd? l)])
    (pp a) (pp b))
  (pp (find odd? l))
  (pp (find-tail even? l))
  (pp (take-while odd? l))
  (pp (drop-while odd? l))
  (receive [a b] (span odd? l)
    (pp a) (pp b))
  (receive [a b] (break even? l)
    (pp a) (pp b))
  (pp (any odd? l))
  (pp (every odd? l))
  (pp (list-index (lambda (e) (= e 3)) l))
  (pp (memv 3 l))
  (pp (delete 3 '(1 2 3 4 3 5 3 6)))
  (pp (delete-duplicates '(1 2 3 4 3 5 3 6)))
  (let* ([al '((a . 1) (b . 2) (c . 3))]
         [al2 (alist-cons 'd 4 al)])
    (pp (assq 'd al2))))
