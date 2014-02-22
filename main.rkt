#lang racket/base
(require (for-syntax racket/base))
(require racket/string racket/list racket/vector)
(require "patterns.rkt" "exceptions.rkt" txexpr xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hyphenate module
;;; Racket port of Ned Batchelder's hyphenate.py
;;; http://nedbatchelder.com/code/modules/hyphenate.html
;;; (in the public domain)
;;; which in turn was an implementation
;;; of the Liang hyphenation algorithm in TeX
;;; (also in the public domain)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ safe (require racket/contract))

(define-syntax (define+provide+safe stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide+safe proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (define name body ...)
         (provide name)
         (module+ safe 
           (provide (contract-out [name contract]))))]))

;; module data, define now but set! them later (because they're potentially big & slow)
(define exceptions #f)
(define pattern-tree #f)
;; module default values
(define default-min-length 5)
(define default-joiner (integer->char #x00AD))


;; Convert the hyphenated pattern into a point array for use later.
(define (vector->exceptions exn-strings) 
  (define (make-key x)
    (string-replace x "-" ""))
  
  (define (make-value x)
    (list->vector (cons 0 (map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"[a-z]" x)))))
  
  (make-hash (vector->list (vector-map (λ(x) (cons (make-key x) (make-value x))) exn-strings))))

;; An exception-word is a string of word characters or hyphens.
(define (exception-word? x)
  (if (regexp-match #px"^[\\w-]+$" x) #t #f))

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
  (vector-map insert-pattern pattern-data)
  tree)



(define (make-points word) 
  
  (define (make-zeroes points)
    ; controls hyphenation zone from edges of word
    ; possible todo: make this user-configurable?
    (vector-map (λ(i) (vector-set! points i 0)) (vector 1 2 (- (vector-length points) 2) (- (vector-length points) 3)))
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

;; helper macro that applies proc to all strings found in xexpr input
(define-syntax (apply-xexpr-strings stx)
  (syntax-case stx ()
    [(_ proc val) #'(let loop ([x val])
                      (cond
                        [(string? x) (proc x)]
                        [(txexpr? x) (map-elements loop x)]
                        [else x]))]))

;; Hyphenate using a filter procedure.
(define+provide+safe (hyphenatef x proc [joiner default-joiner] 
                                     #:exceptions [extra-exceptions '()]  
                                     #:min-length [min-length default-min-length])
  ((xexpr? procedure?) ((or/c char? string?) 
                        #:exceptions (listof exception-word?) 
                        #:min-length (or/c integer? #f)) . ->* . xexpr/c)
  
  ;; set up module data
  ;; todo?: change set! to parameterize
  (set! exceptions (vector->exceptions (vector-append default-exceptions (list->vector extra-exceptions))))
  (when (not pattern-tree) (set! pattern-tree (make-pattern-tree default-patterns)))
  
  (define joiner-string (joiner->string joiner))
  (define word-pattern #px"\\w+") ;; more restrictive than exception-word
  ;; todo?: connect this regexp pattern to the one used in word? predicate
  (define (insert-hyphens text)
    (regexp-replace* word-pattern text (λ(word) (if (proc word) (string-join (word->hyphenation-points word min-length) joiner-string) word))))
  
  (apply-xexpr-strings insert-hyphens x))


;; Default hyphenate is a special case of hyphenatef.
(define+provide+safe (hyphenate x [joiner default-joiner]  
                                    #:exceptions [extra-exceptions '()] 
                                    #:min-length [min-length default-min-length])
  ((xexpr/c) ((or/c char? string?) 
              #:exceptions (listof exception-word?) 
              #:min-length (or/c integer? #f)) . ->* . xexpr/c)
  (hyphenatef x (λ(x) #t) joiner #:exceptions extra-exceptions #:min-length min-length))


;; Remove hyphens.
(define+provide+safe (unhyphenate x [joiner default-joiner])
  ((xexpr/c) ((or/c char? string?)) . ->* . xexpr/c)
  
  (define (remove-hyphens text)
    (string-replace text (joiner->string joiner) ""))
  
  (apply-xexpr-strings remove-hyphens x))  
