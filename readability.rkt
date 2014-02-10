#lang racket/base
(require racket/contract net/url xml)
(require (only-in racket/list empty? range splitf-at dropf dropf-right))
(require (only-in racket/format ~a))
(require (only-in racket/string string-join))
(require (only-in racket/vector vector-member))
(require (only-in racket/set set set->list set?))
(module+ test (require rackunit))

(provide (all-defined-out))



;; general way of coercing to string
(define/contract (->string x)
  (any/c . -> . string?)
  (cond 
    [(string? x) x]
    [(empty? x) ""]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [(path? x) (path->string x)]
    [(char? x) (~a x)]
    [(xexpr? x) (xexpr->string x)] ; put this last so other xexprish things don't get caught
    [else (error (format "Can't make ~a into string" x))]))

(module+ test
  (check-equal? (->string "foo") "foo")
  (check-equal? (->string '()) "")
  (check-equal? (->string 'foo) "foo")
  (check-equal? (->string 123) "123")
  (define file-name-as-text "foo.txt")
  (check-equal? (->string (string->path file-name-as-text)) file-name-as-text)
  (check-equal? (->string #\¶) "¶")
  (check-equal? (->string '(foo "bar")) "<foo>bar</foo>"))







;; general way of coercing to a list
(define/contract (->list x)
  (any/c . -> . list?)
  (cond 
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [(set? x) (set->list x)]
    [else (list x)])) 

(module+ test
  (check-equal? (->list '(1 2 3)) '(1 2 3))
  (check-equal? (->list (list->vector '(1 2 3))) '(1 2 3))
  (check-equal? (->list (set 1 2 3)) '(3 2 1))
  (check-equal? (->list "foo") (list "foo")))

;; general way of coercing to vector
(define (->vector x)
  (any/c . -> . vector?)
  ; todo: on bad input, it will pop a list error rather than vector error
  (cond
    [(vector? x) x]
    [else (list->vector (->list x))]))



;; general way of coercing to boolean
(define/contract (->boolean x)
  (any/c . -> . boolean?)
  ;; in Racket, everything but #f is true
  (if x #t #f))

(module+ test
  (check-true (->boolean #t))
  (check-false (->boolean #f))
  (check-true (->boolean "#f")) 
  (check-true (->boolean "foo"))
  (check-true (->boolean '()))
  (check-true (->boolean '(1 2 3))))


(define/contract (has-length? x)
  (any/c . -> . boolean?)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector? hash? set?)))

;; general way of asking for length
(define/contract (len x)
  (has-length? . -> . integer?)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (->string x))]
    [(vector? x) (len (->list x))]
    [(hash? x) (len (hash-keys x))]
    [(set? x) (len (->list x))]
    [else #f]))

(module+ test
  (check-equal? (len '(1 2 3)) 3)
  (check-not-equal? (len '(1 2)) 3) ; len 2
  (check-equal? (len "foo") 3)
  (check-not-equal? (len "fo") 3) ; len 2
  (check-equal? (len 'foo) 3)
  (check-not-equal? (len 'fo) 3) ; len 2
  (check-equal? (len (list->vector '(1 2 3))) 3)
  (check-not-equal? (len (list->vector '(1 2))) 3) ; len 2
  (check-equal? (len (set 1 2 3)) 3)
  (check-not-equal? (len (set 1 2)) 3) ; len 2
  (check-equal? (len (make-hash '((a . 1) (b . 2) (c . 3)))) 3)
  (check-not-equal? (len (make-hash '((a . 1) (b . 2)))) 3)) ; len 2






;; trim from beginning & end of list
(define (trim items test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf items test-proc) test-proc))

(module+ test
  ;  (check-equal? (trim (list "\n" " " 1 2 3 "\n") whitespace?) '(1 2 3))
  (check-equal? (trim (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8)))



;; split list into list of sublists using test-proc
(define/contract (splitf-at* xs split-test)
  ;; todo: better error message when split-test is not a predicate
  (list? predicate/c . -> . (listof list?))
  (define (&splitf-at* xs [acc '()]) ; use acc for tail recursion
    (if (empty? xs) 
        ;; reverse because accumulation is happening backward 
        ;; (because I'm using cons to push latest match onto front of list)
        (reverse acc)
        (let-values ([(item rest) 
                      ;; drop matching elements from front
                      ;; then split on nonmatching 
                      ;; = nonmatching item + other elements (which will start with matching)
                      (splitf-at (dropf xs split-test) (compose1 not split-test))])
          ;; recurse, and store new item in accumulator
          (&splitf-at* rest (cons item acc)))))
  
  ;; trim off elements matching split-test
  (&splitf-at* (trim xs split-test)))

(module+ test
  ; (check-equal? (splitf-at* '("foo" " " "bar" "\n" "\n" "ino") whitespace?) '(("foo")("bar")("ino")))
  (check-equal? (splitf-at* '(1 2 3 4 5 6) even?) '((1)(3)(5))))


