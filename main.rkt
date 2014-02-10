#lang racket/base
(require racket/string racket/list racket/contract racket/vector racket/bool)
(require "data.rkt" "readability.rkt")

(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hyphenate.rkt
;;; Racket port of Ned Batchelder's hyphenate.py
;;; http://nedbatchelder.com/code/modules/hyphenate.html
;;; (in the public domain)
;;; which in turn was an implementation
;;; of the Liang hyphenation algorithm in TeX
;;; (also in the public domain)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide hyphenate hyphenatef unhyphenate)

;; global data, define now but set! them later (because they're potentially big & slow)
(define exceptions #f)
(define pattern-tree #f)
(define default-min-length 5)

;; Convert the hyphenated pattern into a point array for use later.
(define/contract (list->exceptions exn-strings) 
  ((listof string?) . -> . hash?)
  (define (make-key x)
    (string-replace x "-" ""))
  
  (define (make-value x)
    (list->vector (cons 0 (map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"[a-z]" x)))))
  
  (make-hash (map (λ(x) (cons (make-key x) (make-value x))) exn-strings)))



;; A word is a string without whitespace.
(define/contract (word? x)
  (any/c . -> . boolean?)
  (->boolean (regexp-match #px"^\\S+$" x)))

(module+ test
  (check-true (word? "Foobar"))
  (check-true (word? "foobar"))
  (check-true (word? "foo-bar"))
  (check-false (word? "foo bar")))

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
    (let* ([chars (regexp-replace* #px"[0-9]" pat "")]
           ;; regexp returns list of strings
           [points (map (λ(x) (if (> (len x) 0) (string->number x) 0)) (regexp-split #px"[.a-z]" pat))]
           [tree tree])
      (for ([char chars])
        (when (not (hash-has-key? tree char))
          (hash-set! tree char (make-hash)))
        (set! tree (hash-ref tree char)))
      (hash-set! tree empty points)))
  (map insert-pattern pattern-data)
  tree)



(define/contract (make-points word) 
  (word? . -> . vector?)
  
  (define/contract (make-zeroes points)
    (vector? . -> . vector?)
    ; controls hyphenation zone from edges of word
    ; todo: parameterize this setting
    ; todo: does this count end-of-word punctuation? it shouldn't.
    (map (λ(i) (vector-set! points i 0)) (list 1 2 (- (len points) 2) (- (len points) 3)))
    points)
  
  (let* ([word (string-downcase word)]
         [points 
          (if (hash-has-key? exceptions word)
              (hash-ref exceptions word)
              (let* ([work (string-append "." (->string word) ".")]
                     [points (make-vector (add1 (len work)) 0)]) 
                (for ([i (len work)])
                  (let ([tree pattern-tree])
                    (for ([char (substring work i (len work))]
                          #:break (not (hash-has-key? tree char)))
                      (set! tree (hash-ref tree char))
                      (when (hash-has-key? tree empty)
                        (let ([point (hash-ref tree empty)])
                          (for ([j (len point)])
                            (vector-set! points (+ i j) (max (vector-ref points (+ i j)) (list-ref point j)))))))))
                points))])
    
    ; make-zeroes controls minimum hyphenation distance from edge.
    ; todo: dropping first 2 elements is needed for mysterious reasons to be documented later
    (vector-drop (make-zeroes points) 2)))



;; Find hyphenatable pieces of a word. This is not quite synonymous with syllables.
(define/contract (word->pieces word [min-length default-min-length])
  ((word?) (integer?) . ->* . (listof string?))  
  
  (define (make-pieces word)
    (define word-dissected (flatten (for/list ([char word] 
                                               [point (make-points word)])
                                      (if (even? point)
                                          char ; even point denotes character
                                          (cons char 'syllable))))) ; odd point denotes char + syllable
    (map list->string (splitf-at* word-dissected symbol?)))
  
  (if (< (len word) min-length)
      (list word)  
      (make-pieces word)))

(define default-joiner (integer->char #x00AD))

;; Hyphenate using a filter procedure.
;; Theoretically possible to do this externally,
;; but it would just mean doing the regexp-replace twice.
(define/contract (hyphenatef text proc [joiner default-joiner] #:exceptions [extra-exceptions '()]  #:min-length [min-length default-min-length])
  ((string? procedure?) ((or/c char? string?) #:exceptions (listof word?) #:min-length (or/c integer? false?)) . ->* . string?)
  
  ;; set up module data
  (set! exceptions (list->exceptions (append default-exceptions (map ->string extra-exceptions))))
  (when (not pattern-tree) (set! pattern-tree (make-pattern-tree default-patterns)))
  
  (regexp-replace* #px"\\w+" text (λ(word) (if (proc word) (string-join (word->pieces word min-length) (->string joiner)) word))))


;; Default hyphenate function.
(define/contract (hyphenate text [joiner default-joiner]  #:exceptions [extra-exceptions '()] #:min-length [min-length default-min-length])
  ((string?) ((or/c char? string?) #:exceptions (listof word?) #:min-length (or/c integer? false?)) . ->* . string?) 
  (hyphenatef text (λ(x) #t) joiner #:exceptions extra-exceptions #:min-length min-length))

(define/contract (unhyphenate text [joiner default-joiner])
  ((string?) ((or/c char? string?)) . ->* . string?) 
  (string-replace text (->string joiner) ""))


(module+ test
  (check-equal? (hyphenate "polymorphism") "poly\u00ADmor\u00ADphism")
  (check-equal? (hyphenate "polymorphism" #:min-length 100) "polymorphism")
  (check-equal? (hyphenate "ugly" #:min-length 1) "ug\u00ADly")
  (check-equal? (unhyphenate "poly\u00ADmor\u00ADphism") "polymorphism")
  (check-equal? (hyphenatef "polymorphism" (λ(x) #f)) "polymorphism")
  (check-equal? (hyphenate "polymorphism" #\-) "poly-mor-phism")
  (check-equal? (hyphenate "polymorphism" "foo") "polyfoomorfoophism")
    (check-equal? (unhyphenate "polyfoomorfoophism" "foo") "polymorphism")
  (check-equal? (hyphenate "polymorphism" #\* #:exceptions '("polymo-rphism")) "polymo*rphism")
  
  (check-equal? (hyphenate "circular polymorphism squandering") "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism squan\u00ADder\u00ADing")
  (check-equal? (hyphenate "present project") "present project") ; exception words
  ;; test these last so exceptions have been set up already
  (check-equal? (word->pieces "polymorphism") '("poly" "mor" "phism"))
  (check-equal? (word->pieces "present") '("present"))) ; exception word
