#lang racket/base
(require (for-syntax racket/base))
(require racket/string racket/list racket/contract racket/vector)
(require "patterns.rkt" "exceptions.rkt" tagged-xexpr xml)

(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hyphenate module
;;; Racket port of Ned Batchelder's hyphenate.py
;;; http://nedbatchelder.com/code/modules/hyphenate.html
;;; (in the public domain)
;;; which in turn was an implementation
;;; of the Liang hyphenation algorithm in TeX
;;; (also in the public domain)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define+provide/contract stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide/contract proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (provide (contract-out [name contract]))
         (define name body ...))]))

;; global data, define now but set! them later (because they're potentially big & slow)
(define exceptions #f)
(define pattern-tree #f)
;; global default values
(define default-min-length 5)
(define default-joiner (integer->char #x00AD))


;; Convert the hyphenated pattern into a point array for use later.
(define (list->exceptions exn-strings) 
  (define (make-key x)
    (string-replace x "-" ""))
  
  (define (make-value x)
    (list->vector (cons 0 (map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"[a-z]" x)))))
  
  (make-hash (map (λ(x) (cons (make-key x) (make-value x))) exn-strings)))



;; An exception-word is a string of word characters or hyphens.
(define (exception-word? x)
  (if (regexp-match #px"^[\\w-]+$" x) #t #f))

(module+ test
  (check-true (exception-word? "Foobar"))
  (check-true (exception-word? "foobar"))
  (check-false (exception-word? "foobar!"))
  (check-true (exception-word? "foo-bar"))
  (check-false (exception-word? "foo bar")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert a pattern like 'a1bc3d4' into a string of chars 'abcd'
;; and a list of points [ 1, 0, 3, 4 ].
(define (make-pattern-tree pattern-data)
  (define tree (make-hash))
  
  ;; Insert the pattern into the tree.  Each character finds a dict
  ;; another level down in the tree, and leaf nodes have the list of
  ;; points.
  (define (insert-pattern pat)
    ;; todo?: filter other characters out of input string?
    (let* ([chars (regexp-replace* #px"[0-9]" pat "")]
           ;; regexp returns list of strings
           [points (map (λ(x) (if (> (string-length x) 0) (string->number x) 0)) (regexp-split #px"[.a-z]" pat))]
           [tree tree])
      (for ([char chars])
        (when (not (hash-has-key? tree char))
          (hash-set! tree char (make-hash)))
        (set! tree (hash-ref tree char)))
      (hash-set! tree empty points)))
  (map insert-pattern pattern-data)
  tree)



(define (make-points word) 
  
  (define (make-zeroes points)
    ; controls hyphenation zone from edges of word
    ; possible todo: make this user-configurable?
    (map (λ(i) (vector-set! points i 0)) (list 1 2 (- (vector-length points) 2) (- (vector-length points) 3)))
    points)
  
  (let* ([word (string-downcase word)]
         [points 
          (if (hash-has-key? exceptions word)
              (hash-ref exceptions word)
              (let* ([work (string-append "." word ".")]
                     [points (make-vector (add1 (string-length work)) 0)]) 
                (for ([i (string-length work)])
                  (let ([tree pattern-tree])
                    (for ([char (substring work i (string-length work))]
                          #:break (not (hash-has-key? tree char)))
                      (set! tree (hash-ref tree char))
                      (when (hash-has-key? tree empty)
                        (let ([point (hash-ref tree empty)])
                          (for ([j (length point)])
                            (vector-set! points (+ i j) (max (vector-ref points (+ i j)) (list-ref point j)))))))))
                points))])
    
    ; make-zeroes controls minimum hyphenation distance from edge.
    ; todo: dropping first 2 elements is needed for mysterious reasons to be documented later
    (vector-drop (make-zeroes points) 2)))


;; helpful extension of splitf-at
(define (splitf-at* xs split-test)
  
  (define (trim items test-proc)
    (dropf-right (dropf items test-proc) test-proc))
  
  (define (&splitf-at* xs [acc '()])
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


;; Find hyphenation points in a word. This is not quite synonymous with syllables.
(define (word->hyphenation-points word [min-length default-min-length])
  
  (define (make-pieces word)
    (define word-dissected (flatten (for/list ([char word] 
                                               [point (make-points word)])
                                      (if (even? point)
                                          char ; even point denotes character
                                          (cons char 'syllable))))) ; odd point denotes char + syllable
    (map list->string (splitf-at* word-dissected symbol?)))
  
  (if (and min-length (< (string-length word) min-length))
      (list word)  
      (make-pieces word)))


;; joiner contract allows char or string; this coerces to string.
(define (joiner->string joiner)
  (if (char? joiner) (format "~a" joiner) joiner))

;; Hyphenate using a filter procedure.
(define+provide/contract (hyphenatef x proc [joiner default-joiner] 
                                     #:exceptions [extra-exceptions '()]  
                                     #:min-length [min-length default-min-length])
  ((xexpr? procedure?) ((or/c char? string?) 
                        #:exceptions (listof exception-word?) 
                        #:min-length (or/c integer? #f)) . ->* . string?)
  
  ;; set up module data
  ;; todo?: change set! to parameterize
  (set! exceptions (list->exceptions (append default-exceptions extra-exceptions)))
  (when (not pattern-tree) (set! pattern-tree (make-pattern-tree default-patterns)))
  
  (define joiner-string (joiner->string joiner))
  (define word-pattern #px"\\w+") ;; more restrictive than exception-word
  ;; todo?: connect this regexp pattern to the one used in word? predicate
  (define (insert-hyphens text)
    (regexp-replace* word-pattern text (λ(word) (if (proc word) (string-join (word->hyphenation-points word min-length) joiner-string) word))))
  
  (let &hyphenate ([x x])
    (cond
      [(string? x) (insert-hyphens x)]
      [(tagged-xexpr? x) (map-elements &hyphenate x)]
      [else x])))


;; Default hyphenate function.
(define+provide/contract (hyphenate x [joiner default-joiner]  
                                    #:exceptions [extra-exceptions '()] 
                                    #:min-length [min-length default-min-length])
  ((xexpr/c) ((or/c char? string?) 
             #:exceptions (listof exception-word?) 
             #:min-length (or/c integer? #f)) . ->* . string?)
  (hyphenatef x (λ(x) #t) joiner #:exceptions extra-exceptions #:min-length min-length))

(define+provide/contract (unhyphenate x [joiner default-joiner])
  ((xexpr/c) ((or/c char? string?)) . ->* . string?)
  
  (define (remove-hyphens text)
    (string-replace text (joiner->string joiner) ""))
  
  (let &unhyphenate ([x x])
    (cond
      [(string? x) (remove-hyphens x)]
      [(tagged-xexpr? x) (map-elements &unhyphenate x)]
      [else x])))
  
  
  
  