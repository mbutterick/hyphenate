#lang racket/base
(require txexpr/base racket/string racket/list "params.rkt")
(provide hyphenate unhyphenate word->hyphenation-points convert-exception-word string->hashpair)

(module+ test
  (require rackunit))


;; module default values
(define default-min-length 5)
(define default-min-left-length 2)
(define default-min-right-length 2)
(define default-joiner #\u00AD)


(define (convert-exception-word ew)
  ;; an exception word indicates its breakpoints with added hyphens
  (define word (string-replace ew "-" ""))
  ;; pattern has same number of points as word letters. 1 marks hyphenation point; 0 no hyphenation
  (define points
    (cdr (map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"\\p{L}" ew))))
  ;; use list here so we can `apply` in `add-exception-word`
  (list word points))


(module+ test
  (check-equal? (convert-exception-word "snôw-man") '("snôwman" (0 0 0 1 0 0 0)))
  (check-equal? (convert-exception-word "snôwman") '("snôwman" (0 0 0 0 0 0 0)))
  (check-equal? (convert-exception-word "sn-ôw-ma-n") '("snôwman" (0 1 0 1 0 1 0))))


(define (add-exception-word word)
  ;; `hash-set!` not `hash-ref!`, because we want an exception to override an existing value
  (apply hash-set! (current-word-cache) (convert-exception-word word)))


(define (string->natural i)
  (let* ([result (string->number i)]
         [result (and result (inexact->exact result))]
         [result (and (exact-nonnegative-integer? result) result)])
    result))


(module+ test
  (check-equal? (string->natural "Foo") #f)
  (check-equal? (string->natural "1.0") 1)
  (check-equal? (string->natural "-3") #f))


(define (string->hashpair pat)
  ;; first convert the pattern to a list of alternating letters and numbers.
  ;; insert zeroes where there isn't a number in the pattern.
  (define-values (strs nums)
    (for/lists (strs nums)
               ;; using unicode-aware regexps to allow unicode hyphenation patterns
               ;; a pattern is a list of subpatterns, each of which is a character possibly followed by a number.
               ;; also, the first position may just have a number.
               ([subpat (in-list (regexp-match* #px"^\\d?|(\\p{L}|\\p{P})\\d?" pat))])
               (define str (cond
                             [(regexp-match #px"(\\p{L}|\\p{P})?" subpat) => car]
                             [else ""]))
               (define num (cond
                             [(regexp-match #px"\\d" subpat) => (compose1 string->natural car)]
                             [else 0]))
               (values str num)))
  (list (string-append* strs) nums))


(module+ test
  (check-equal? (string->hashpair "2'2") '("'" (2 2)))
  (check-equal? (string->hashpair ".â4") '(".â" (0 0 4)))
  (check-equal? (string->hashpair ".ý4") '(".ý" (0 0 4)))
  (check-equal? (string->hashpair "'ý4") '("'ý" (0 0 4)))
  (check-equal? (string->hashpair "’ý4") '("’ý" (0 0 4)))
  (check-equal? (string->hashpair "4ý-") '("ý-" (4 0 0)))
  (check-equal? (string->hashpair ".ach4") '(".ach" (0 0 0 0 4))))


(define (calculate-max-pattern patterns)
  ;; each pattern is a list of numbers
  ;; all the patterns have the same length
  ;; run max against parallel elements
  (apply map (λ xs (apply max xs)) patterns)) 


(module+ test
  (require rackunit)
  (check-equal? (calculate-max-pattern '((1 0 0))) '(1 0 0))
  (check-equal? (calculate-max-pattern '((1 0 0) (0 1 0))) '(1 1 0))
  (check-equal? (calculate-max-pattern '((1 0 0) (0 1 0) (0 0 1))) '(1 1 1))
  (check-equal? (calculate-max-pattern '((1 2 3) (2 3 1) (3 1 2))) '(3 3 3)))


(define (make-points word)
  (hash-ref! (current-word-cache) word 
             (λ () ; compute pattern when missing from cache
               (define word-with-dots (format ".~a." (string-downcase word)))
               (define word-length (string-length word-with-dots))
               (define default-zero-pattern (make-list (add1 word-length) 0))
               ;; walk through all the substrings and see if there's a matching pattern.
               (define matching-patterns
                 (for*/list ([start (in-range word-length)] 
                             [end (in-range start word-length)]
                             [substr (in-value (substring word-with-dots start (add1 end)))]
                             [partial-pattern (in-value (hash-ref (current-patterns) substr #f))]
                             #:when partial-pattern)
                            ;; pad out partial-pattern to full length
                            ;; (so we can compare patterns to find max value for each slot)
                            (define left-zeroes (make-list start 0))
                            (define right-zeroes (make-list (- (add1 word-length) (length partial-pattern) start) 0))
                            (append left-zeroes partial-pattern right-zeroes)))
               (define max-pattern (calculate-max-pattern (cons default-zero-pattern matching-patterns)))
               ;; for point list generated from a pattern,
               ;; drop first two elements because they represent hyphenation weight
               ;; before the starting "." and between "." and the first letter.
               ;; drop last element because it represents hyphen after last "."
               ;; after you drop these two, then each number corresponds to
               ;; whether a hyphen goes after that letter.
               (drop-right (drop max-pattern 2) 1))))


;; Find hyphenation points in a word. This is not quite synonymous with syllables.
(define (word->hyphenation-points word 
                                  [min-length #f] 
                                  [min-left-length #f] 
                                  [min-right-length #f])
  
  (cond
    [(< (string-length word) (or min-length default-min-length)) (list word)]
    [else
     ;; points is a list corresponding to the letters of the word.
     ;; to create a no-hyphenation zone of length n, zero out the first n-1 points
     ;; and the last n points (because the last value in points is always superfluous)
     (define word-points
       (let* ([points (make-points word)]
              [left-zeroes (min (or min-left-length default-min-left-length) (length points))]
              [right-zeroes (min (or min-right-length default-min-right-length) (length points))])
         (for/list ([(point idx) (in-indexed points)])
                   (if (<= left-zeroes (add1 idx) (- (length points) right-zeroes))
                       point
                       0))))
     
     ;; odd-valued points in the pattern denote hyphenation points
     (define odd-point-indexes (for/list ([(wp idx) (in-indexed word-points)]
                                   #:when (odd? wp))
                                  idx))
     
     ;; the hyphenation goes after the indexed letter, so add1 to the raw points for slicing
     (define breakpoints (append (list 0) (map add1 odd-point-indexes) (list (string-length word))))
     (for/list ([start (in-list breakpoints)]
                [end (in-list (cdr breakpoints))]) ; shorter list controls exit of loop
               (substring word start end))]))


;; joiner contract allows char or string; this coerces to string.
(define (joiner->string joiner) (format "~a" joiner))


(define (apply-proc proc x [omit-string (λ(x) #f)] [omit-txexpr (λ(x) #f)])
  (let loop ([x x])
    (cond
      [(and (string? x) (not (omit-string x))) (proc x)]
      [(and (txexpr? x) (not (omit-txexpr x))) 
       (make-txexpr (get-tag x) (get-attrs x) (map loop (get-elements x)))]
      [else x])))


(define (hyphenate x [joiner default-joiner] 
                   #:exceptions [extra-exceptions empty]  
                   #:min-length [min-length default-min-length]
                   #:min-left-length [min-left-length default-min-left-length]
                   #:min-right-length [min-right-length default-min-right-length]
                   #:omit-word [omit-word? (λ(x) #f)]
                   #:omit-string [omit-string? (λ(x) #f)]
                   #:omit-txexpr [omit-txexpr? (λ(x) #f)])
  
  ;; todo?: connect this regexp pattern to the one used in word? predicate
  (for-each add-exception-word extra-exceptions)
  (define word-pattern #px"\\w+") ;; more restrictive than exception-word
  (define (replacer word . words)
    (if (not (omit-word? word)) 
        (string-join (word->hyphenation-points word min-length min-left-length min-right-length) (joiner->string joiner)) 
        word))
  (define (insert-hyphens text) (regexp-replace* word-pattern text replacer))  
  (apply-proc insert-hyphens x omit-string? omit-txexpr?))


(define (unhyphenate x [joiner default-joiner] 
                     #:omit-word [omit-word? (λ(x) #f)]
                     #:omit-string [omit-string? (λ(x) #f)]
                     #:omit-txexpr [omit-txexpr? (λ(x) #f)])
  (define word-pattern (pregexp (format "[\\w~a]+" joiner)))
  (define (replacer word . words)
    (if (not (omit-word? word)) 
        (string-replace word (joiner->string joiner) "")
        word))
  (define (remove-hyphens text) (regexp-replace* word-pattern text replacer))
  (apply-proc remove-hyphens x omit-string? omit-txexpr?))  
