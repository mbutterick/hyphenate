#lang racket/base
(require (for-syntax racket/base))
(require racket/string racket/list racket/bool)
(require "patterns-hashed.rkt" "exceptions.rkt" txexpr xml)

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
(define patterns #f)
(define pattern-cache #f)

;; module default values
(define default-min-length 5)
(define default-min-left-length 2)
(define default-min-right-length 2)
(define default-joiner #\u00AD)

(define (add-pattern-to-cache pat)
  (hash-set! pattern-cache (car pat) (cdr pat)))

(define (initialize-patterns)
  (when (not pattern-cache)
    (set! pattern-cache (make-hash))
    (for-each (compose1 add-exception symbol->string) default-exceptions))
  (when (not patterns)
    ;(set! patterns (make-hash (map (compose1 string->hashpair symbol->string) default-patterns))))
    (set! patterns hashed-patterns)))

;; Convert the hyphenated pattern into a point array for use later.
(define (add-exception exception) 
  (define (make-key x) (format ".~a." (string-replace x "-" "")))
  (define (make-value x) `(0 ,@(map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"[a-z]" x)) 0))
  (add-pattern-to-cache (cons (make-key exception) (make-value exception)))
  (void))

;; An exception-word is a string of word characters or hyphens.
(define (exception-word? x)
  (if (regexp-match #px"^[\\w-]+$" x) #t #f))

(define (exception-words? xs) (and (list? xs) (andmap exception-word? xs)))

(define (string->hashpair pat)
  (define boundary-name ".")
  
  ;; first convert the pattern to a list of alternating letters and numbers.
  ;; insert zeroes where there isn't a number in the pattern.
  (define new-pat
    (let* ([pat (regexp-match* #rx"." pat)] ; convert to list
           [pat (map (λ(i) (or (string->number i) i)) pat)] ; convert numbers
           [pat (if (string? (car pat)) (cons 0 pat) pat)] ; add zeroes to front where needed
           [pat (if (string? (car (reverse pat))) (reverse (cons 0 (reverse pat))) pat)]) ; and back
      (flatten (for/list ([(current i) (in-indexed pat)])
                 (if (= i (sub1 (length pat))) 
                    current
                    (let ([next (list-ref pat (add1 i))])
                      ;; insert zeroes where there isn't a number
                      (if (and (or (equal? current boundary-name) (string? current)) (string? next)) 
                         (list current 0)
                         current)))))))
  
  ;; then slice out the string & numerical parts to be a key / value pair.
  (define-values (value key) (partition number? new-pat))
  (cons (apply string-append key) value))



(define (make-points word)
  ;; walk through all the substrings and see if there's a matching pattern.
  ;; if so, pad it out to full length (so we can (apply map max ...) later on)
  (define word-with-dots (format ".~a." (string-downcase word)))
  (define matching-patterns 
    (if (hash-has-key? pattern-cache word-with-dots)
       (list (hash-ref pattern-cache word-with-dots))
       (let ([word-as-list (string->list word-with-dots)])
         (cons (make-list (add1 (length word-as-list)) 0) ;; ensures there's at least one (null) element in return value
              (filter-not void?
                         (for*/list ([len (in-range (length word-as-list))] [index (in-range (- (length word-as-list) len))])
                           (define substring (list->string (take (drop word-as-list index) (add1 len))))
                           (when (hash-has-key? patterns substring)
                             (define value (hash-ref patterns substring))
                             ;; put together head padding + value + tail padding
                             (append  (make-list index 0)  value  (make-list (- (add1 (length word-as-list)) (length value) index) 0))))))))) 
  
  (define max-value-pattern (apply map max matching-patterns))
  (add-pattern-to-cache (cons word-with-dots max-value-pattern))
  
  ;; for point list,
  ;; drop first two elements because they represent hyphenation weight
  ;; before the starting "." and between "." and the first letter.
  ;; drop last element because it represents hyphen after last "."
  ;; after you drop these two, then each number corresponds to
  ;; whether a hyphen goes after that letter.
  (drop-right (drop max-value-pattern 2) 1))


;; helpful extension of splitf-at
(define (filter-split xs split-test)
  
  (define (trimf items test-proc)
    (dropf-right (dropf items test-proc) test-proc))
  
  (define (&filter-split xs [acc '()])
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
         (&filter-split rest (cons item acc)))))
  
  ;; trim off elements matching split-test
  (&filter-split (trimf xs split-test)))


;; Find hyphenation points in a word. This is not quite synonymous with syllables.
(define (word->hyphenation-points word [min-length default-min-length] 
                                  [min-left-length default-min-left-length]
                                  [min-right-length default-min-right-length])
  
  (define (add-no-hyphen-zone points)
    ; points is a list corresponding to the letters of the word.
    ; to create a no-hyphenation zone of length n, zero out the first n-1 points
    ; and the last n points (because the last value in points is always superfluous)
    (let* ([min-left-length (min (or min-left-length default-min-left-length) (length points))]
           [min-right-length (min (or min-right-length default-min-right-length) (length points))])
      (define points-with-zeroes-on-left (append (make-list (sub1 min-left-length) 0) (drop points (sub1 min-left-length))))
      (define points-with-zeroes-on-left-and-right (append (drop-right points-with-zeroes-on-left min-right-length) (make-list min-right-length 0)))
      points-with-zeroes-on-left-and-right))
  
  (define (make-pieces word)
    (define word-dissected (flatten (for/list ([char (in-string word)] 
                                               [point (in-list (add-no-hyphen-zone (make-points word)))])
                                      (if (even? point)
                                         char ; even point denotes character
                                         (cons char 'syllable))))) ; odd point denotes char + syllable
    (map list->string (filter-split word-dissected symbol?)))
  
  (if (and min-length (< (string-length word) min-length))
     (list word)  
     (make-pieces word)))


;; joiner contract allows char or string; this coerces to string.
(define (joiner->string joiner)
  (if (char? joiner) (format "~a" joiner) joiner))

(define (apply-proc proc x [omit-string (λ(x) #f)] [omit-txexpr (λ(x) #f)])
  ;  ((procedure? txexpr?) ((or/c null (listof txexpr-tag?))) . ->* . txexpr?)
  (let loop ([x x])
    (cond
      [(and (string? x) (not (omit-string x))) (proc x)]
      [(and (txexpr? x) (not (omit-txexpr x))) (cons (car x) (map loop (cdr x)))]
      [else x])))

(define+provide+safe (hyphenate x [joiner default-joiner] 
                               #:exceptions [extra-exceptions '()]  
                               #:min-length [min-length default-min-length]
                               #:min-left-length [min-left-length default-min-left-length]
                               #:min-right-length [min-right-length default-min-right-length]
                               #:omit-word [omit-word? (λ(x) #f)]
                               #:omit-string [omit-string? (λ(x) #f)]
                               #:omit-txexpr [omit-txexpr? (λ(x) #f)])
  ((xexpr?) ((or/c char? string?) 
             #:exceptions exception-words? 
             #:min-length (or/c integer? #f)
             #:omit-word (string? . -> . any/c)
             #:omit-string (string? . -> . any/c)
             #:omit-txexpr (txexpr? . -> . any/c)
             #:min-left-length (or/c (and/c integer? positive?) #f)
             #:min-right-length (or/c (and/c integer? positive?) #f)) . ->* . xexpr/c)
  
  (initialize-patterns) ; reset everything each time hyphenate is called
  (for-each add-exception extra-exceptions)
  
  (define joiner-string (joiner->string joiner))
  ;; todo?: connect this regexp pattern to the one used in word? predicate
  (define word-pattern #px"\\w+") ;; more restrictive than exception-word
  (define (insert-hyphens text)
    (regexp-replace* word-pattern text (λ(word) (if (not (omit-word? word)) 
                                                   (string-join (word->hyphenation-points word min-length min-left-length min-right-length) joiner-string) 
                                                   word))))
  
  (apply-proc insert-hyphens x omit-string? omit-txexpr?))


(define+provide+safe (unhyphenate x [joiner default-joiner] 
                                 #:omit-word [omit-word? (λ(x) #f)]
                                 #:omit-string [omit-string? (λ(x) #f)]
                                 #:omit-txexpr [omit-txexpr? (λ(x) #f)])
  ((xexpr/c) ((or/c char? string?) 
              #:omit-word (string? . -> . any/c)
              #:omit-string (string? . -> . any/c)
              #:omit-txexpr (txexpr? . -> . any/c)) . ->* . xexpr/c)
  
  (define word-pattern (pregexp (format "[\\w~a]+" joiner)))
  (define (remove-hyphens text)
    (regexp-replace* word-pattern text (λ(word) (if (not (omit-word? word)) 
                                                   (string-replace word (joiner->string joiner) "") 
                                                   word))))
  
  (apply-proc remove-hyphens x omit-string? omit-txexpr?))  


(module+ main
  (initialize-patterns)
  (define t "supercalifragilisticexpialidocious") 
  (hyphenate t "-"))


