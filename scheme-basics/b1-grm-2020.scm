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

;; (let ([p (cons 1 2)]
;;       [p2 (cons 1 2)]
;;       [l (list 1 2 3 4)]
;;       [l2 (list 1 2 3 4)])
;;   (pp p)
;;   (pp l)
;;   (pp (eq? p p2))
;;   (pp (eqv? p p2))
;;   (pp (equal? p p2))
;;   (pp (list-tabulate 5 (lambda (e) (* e 10))))
;;   (pp (iota 5 0 10))
;;   (pp (list= eq? l l2))
;;   (pp (list-ref l 3))
;;   (pp (take l 3))
;;   (pp (drop l 1))
;;   (pp (take-right l 2))
;;   (pp (drop-right l 2))
;;   (let-values ([(a b) (split-at l 1)])
;;     (pp a) (pp b))
;;   (receive (a b) (split-at l 3)
;;     (pp a) (pp b))
;;   (pp (last l))
;;   (pp (last-pair l))
;;   (pp (length l))
;;   (pp (append l l2))
;;   (pp (concatenate (list l l2)))
;;   (pp (reverse l))
;;   (pp (append-reverse l '()))
;;   (pp (apply map list (list l l2)))
;;   (pp (zip l l2))
;;   (pp (zip l))
;;   (let ([ll (zip l)]
;;         [ll2 (zip l l2)])
;;     (pp (unzip1 ll))
;;     (receive [a b] (unzip2 ll2)
;;       (pp a) (pp b)))
;;   (pp (count even? l))
;;   (pp (fold + 0 l))
;;   (pp (fold + 0 (map (const 1) l)))
;;   (pp (fold cons '() l))
;;   (pp (fold-right cons '() l))
;;   (pp (unfold (lambda (e) (> e 10)) (lambda (e) (expt e 2)) (lambda (s) (1+ s)) 1))
;;   (pp (unfold null? car cdr l))
;;   (pp (unfold-right zero? (lambda (e) (expt e 2)) (lambda (s) (1- s)) 10))
;;   (pp (unfold-right null? car cdr l))
;;   (pp (filter odd? l))
;;   (pp (remove odd? l))
;;   (let-values ([(a b) (partition odd? l)])
;;     (pp a) (pp b))
;;   (pp (find odd? l))
;;   (pp (find-tail even? l))
;;   (pp (take-while odd? l))
;;   (pp (drop-while odd? l))
;;   (receive [a b] (span odd? l)
;;     (pp a) (pp b))
;;   (receive [a b] (break even? l)
;;     (pp a) (pp b))
;;   (pp (any odd? l))
;;   (pp (every odd? l))
;;   (pp (list-index (lambda (e) (= e 3)) l))
;;   (pp (memv 3 l))
;;   (pp (delete 3 '(1 2 3 4 3 5 3 6)))
;;   (pp (delete-duplicates '(1 2 3 4 3 5 3 6)))
;;   (let* ([al '((a . 1) (b . 2) (c . 3))]
;;          [al2 (alist-cons 'd 4 al)])
;;     (pp (assq 'd al2))))

;; SRFI-13 String library
;; (let ([s "Vlad and Lana 2020-07-14"])
;;   (pp (string? s))
;;   (pp (string-null? s))
;;   (pp (string-every #\a s))
;;   (pp (string-any #\a s))
;;   (pp (string-tabulate (const #\a) 5))
;;   (pp (list->string (string->list "Vlad")))
;;   (pp (string-join '("Vlad" "and" "Lana") "_"))
;;   (pp (string-length s))
;;   (pp (string-ref s 0))
;;   (pp (string-take s 4))
;;   (pp (string-drop s 4))
;;   (pp (string-pad s 30 #\_))
;;   (pp (string-pad-right s 30 #\_))
;;   (pp (string-trim-both "  Vlad\n\n  "))
;;   (pp (string<> "Vlad" "Lana"))
;;   (pp (string-hash "Vlad" 100))
;;   (pp (string-hash-ci "Vlad" 100))
;;   (pp (string-index s #\a))
;;   (pp (string-skip s #\V))
;;   (pp (string-count s #\a))
;;   (pp (string-contains s "Lana"))
;;   (pp (string-titlecase s))
;;   (pp (string-upcase s))
;;   (pp (string-downcase s))
;;   (pp (string-reverse s))
;;   (pp (string-append "Vlad" " Lana"))
;;   (pp (string-concatenate '("Vlad" " Lana")))
;;   (pp (string-map char-upcase s))
;;   (pp (string-fold-right cons '() s))
;;   (pp (string-unfold null? (lambda (l) (integer->char (car l))) cdr '(1 2 3 4)))
;;   (string-for-each pp "Vlad")
;;   (pp (xsubstring "Vlad" 3))
;;   (pp (string-replace "Vlad and Lana" "AND" 5 8))
;;   (pp (string-tokenize s))
;;   (pp (string-tokenize s (char-set-union char-set:letter char-set:digit)))
;;   (pp (string-filter char-set:digit s))
;;   (pp (string-delete char-set:digit s)))

;; SRFI-14 Charset library
;; (let ([cs (char-set #\a #\b #\c)])
;;   (pp (char-set? cs))
;;   (pp (char-set= cs (char-set #\A #\b #\c)))
;;   (pp (char-set<= cs (char-set #\a #\b #\c #\A)))
;;   (pp (char-set-hash cs))
;;   (pp (char-set-fold cons '() cs))
;;   (pp (char-set-unfold null? car cdr '(#\A #\B #\C)))
;;   (pp (char-set-unfold string-null?
;;                        (lambda (ss) (string-ref ss 0))
;;                        (lambda (ss) (string-drop ss 1)) "DEF"))
;;   (char-set-for-each pp cs)
;;   (pp (char-set-map char-upcase cs))
;;   (pp (char-set-filter char-upper-case? (char-set #\a #\b #\c #\A)))
;;   (pp (char-set-size cs))
;;   (pp (char-set-count char-upper-case? cs))
;;   (pp (char-set-contains? cs #\c))
;;   (pp (char-set-every char-lower-case? cs))
;;   (pp (char-set-any char-upper-case? cs))
;;   (pp (char-set-adjoin (char-set #\a) #\A))
;;   (pp (char-set-delete (char-set #\a #\A) #\A))
;;   (pp (char-set-union (char-set #\a) (char-set #\A)))
;;   (pp (char-set-intersection (char-set #\a #\A) (char-set #\A)))
;;   (pp (char-set-difference (char-set #\a #\b) (char-set #\b)))
;;   (pp (char-set-xor (char-set #\a #\b) (char-set #\A #\b)))
;;   (pp (char-set-size (char-set-complement cs))))

;; (let ([b #t])
;;   (pp (boolean? b))
;;   (pp (not b)))

;; (let ([n 1] [r 1.0] [rr 1/2] [c 1+2i])
;;   (pp (number? n))
;;   (pp (integer? n))
;;   (pp (exact? n))
;;   (pp (inexact? n))
;;   (pp (real? r))
;;   (pp (rational? rr))
;;   (pp (numerator rr))
;;   (pp (denominator rr))
;;   (pp (complex? c))
;;   (pp (inexact->exact 1.0))
;;   (pp (exact->inexact 1))
;;   (pp (odd? 3))
;;   (pp (even? 4))
;;   (pp (quotient -5 3))
;;   (pp (remainder -5 3))
;;   (pp (gcd 12 3))
;;   (pp (lcm 12 3))
;;   (pp (positive? 1))
;;   (pp (negative? -1))
;;   (pp (number->string #x21))
;;   (pp (string->number "41" 8))
;;   (pp (1+ 1))
;;   (pp (1- 1))
;;   (pp (abs -2))
;;   (pp (max 1 2 3))
;;   (pp (min 1 2 3))
;;   (pp (truncate -2.9))
;;   (pp (truncate 2.9))
;;   (pp (floor -2.9))
;;   (pp (floor 2.9))
;;   (pp (ceiling -2.9))
;;   (pp (ceiling 2.9))
;;   (pp (round -2.9))
;;   (pp (round 2.9))
;;   (pp (sqrt 2))
;;   (pp (expt 2 3)))

;; (let ([c #\a])
;;   (pp (char? c))
;;   (pp (char->integer c))
;;   (pp (integer->char 97))
;;   (pp (char-upcase c))
;;   (pp (char-downcase c))
;;   (pp (char-titlecase c)))

;; (let ([s "Vlad"])
;;   (pp (string? s))
;;   (pp (string-null? s))
;;   (pp (string-any char-set:letter s))
;;   (pp (string-every char-set:digit s))
;;   (pp (string->list s))
;;   (pp (list->string '(#\V #\l #\a #\d)))
;;   (pp (string-tabulate (lambda (i) (integer->char (+ i 97))) 10))
;;   (pp (string-join '("Vlad" "Lana") " "))
;;   (pp (string-tokenize
;;        "root:x:0:0:root:/root:/bin/bash"
;;        (char-set-complement (char-set #\:))))
;;   (pp (string-split "root:x:0:0:root:/root:/bin/bash" #\:))
;;   (pp (string-length s))
;;   (pp (string-ref s 0))
;;   (pp (substring s 1))
;;   (pp (xsubstring s 1))
;;   (pp (string-upcase s))
;;   (pp (string-downcase s))
;;   (pp (string-titlecase s))
;;   (pp (string-reverse s))
;;   (pp (string-append "Vlad" " Lana"))
;;   (pp (string-concatenate '("Vlad" " and" " Lana"))))

;; (let ([p '(a . b)])
;;   (pp (pair? p))
;;   (pp (cons 'c 'd))
;;   (pp (car p))
;;   (pp (cdr p))
;;   (set-car! p 'A)
;;   (set-cdr! p 'B)
;;   (pp p))

(use-modules (srfi srfi-1))

;; (let ([l (list 1 2 3 4 5)])
;;   (pp (list? l))
;;   (pp (null? l))
;;   (pp (length l))
;;   (pp (last l))
;;   (pp (last-pair l))
;;   (pp (list-ref l 0))
;;   (pp (reverse l))
;;   (pp (append l '(6 7 8 9)))
;;   (pp (concatenate '((1 2) (3 4)))))

(use-modules (srfi srfi-133))

;; (let ([v #(1 "Vlad" #xff)]
;;       [v2 (vector 1 "Lana" #xff)]
;;       [v3 #(1 2 3 4)])
;;   (pp (vector? v))
;;   (pp (vector 'a 'b 'c))
;;   (pp (list->vector '(1 2 3)))
;;   (pp (vector->list v))
;;   (pp (vector-length v))
;;   (pp (vector-ref v 0))
;;   (vector-set! v2 0 10)
;;   (pp v2)
;;   (pp (make-vector 5 'Vlad))
;;   (pp (vector-unfold (lambda (i s) (values (* s 10) (1- s))) 10 0))
;;   (pp (vector-unfold values 10))
;;   (pp (vector-unfold (lambda (i s) (values (vector-ref s i) s)) (vector-length v) v))
;;   (pp (vector-unfold-right (lambda (i s) (values (vector-ref s i) s)) (vector-length v) v))
;;   (pp (vector-copy v2))
;;   (pp (vector-append v v2))
;;   (pp (vector-concatenate (list v v2)))
;;   (pp (vector-empty? v))
;;   (pp (vector= equal? #(1 #t "S") #(1 #t "S")))
;;   (pp (vector-fold + 0 v3))
;;   (pp (vector-fold max -1 v3))
;;   (pp (vector-fold (lambda (b e) (cons e b))'() v3))
;;   (pp (vector-fold-right (lambda (b e) (cons e b))'() v3))
;;   (pp (vector-map (lambda (e) (* e 10)) v3))
;;   (vector-for-each pp v3)
;;   (pp (vector-count even? v3))
;;   (pp (vector-index odd? v3))
;;   (pp (vector-index-right odd? v3))
;;   (pp (vector-skip odd? v3))
;;   (pp (vector-skip-right odd? v3))
;;   (pp (vector-any even? v3))
;;   (pp (vector-every even? v3))
;;   (receive [v i] (vector-partition even? v3)
;;     (pp v) (pp i))
;;   (vector-swap! v2 0 2)
;;   (pp v2))

(use-modules (ice-9 match)) ;; Pattern matching

(define-record-type <person>
  (make-person name friends)
  person?
  (name person-name)
  (friends person-friends))

;; Pattern matching
;; (pp (letrec ([l '(hello (world))]
;;              [l2 '(1 2 3 4 5)]
;;              [alice (make-person "Alice" (delay (list bob)))]
;;              [bob (make-person "Bob" (delay (list alice)))])
;;       (match l
;;         #;[('hello (who)) who]
;;         [(x y) (list x y)])
;;       (match l2
;;         #;[(h t ...) (list h t)]
;;         #;[m m]
;;         [_ 'ok])
;;       (match alice
;;         #;[($ <person> name (= force (fs))) (list name fs)]
;;         [($ <person> name (= force (($ <person> fname)))) (list name fname)])))

;; (define hello-world
;;   (match-lambda
;;     [('hello (who)) (cons 'hello who)]))

;; (pp (hello-world '(hello (Vlad))))

;; (pp (match-let ([(x y) '(1 2)]
;;                 [(a b) '(a b)])
;;       (list x a y b)))

(pp (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (list a b c))))
(pp (receive (a b c) (values 1 2 3) (list a b c)))
(pp (receive (a . r) (values 1 2 3) (list a r)))
(pp (let-values ([(a b c) (values 1 2 3)]) (list a b c)))
(pp (let-values ([(a . r) (values 1 2 3)]) (list a r)))
