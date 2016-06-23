#lang racket/base
(require txexpr/base racket/string racket/list "params.rkt")
(provide hyphenate unhyphenate word->hyphenation-points convert-exception-word string->hashpair)

;; module default values
(define default-min-length 5)
(define default-min-left-length 2)
(define default-min-right-length 2)
(define default-joiner #\u00AD)

(define (cache-word pat)
  (hash-set! (current-word-cache) (car pat) (cdr pat)))

;; Convert the hyphenated pattern into a point array for use later.
(define (convert-exception-word exception)
  (define (make-key x)
    (format ".~a." (string-replace x "-" "")))
  (define (make-value x)
    `(0 ,@(map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"[a-z]" x)) 0))
  (list (make-key exception) (make-value exception)))


(define (add-exception-word word)
  (current-exceptions (apply hash-set (current-exceptions) (convert-exception-word word))))


(define (string->natural i)
  (let* ([result (string->number i)]
         [result (and result (inexact->exact result))]
         [result (and (exact-nonnegative-integer? result) result)])
    result))


(define (string->hashpair pat)
  ;; first convert the pattern to a list of alternating letters and numbers.
  ;; insert zeroes where there isn't a number in the pattern.
  (define-values (strs nums)
    (for/lists (strs nums)
               ;; using unicode-aware regexps to allow unicode hyphenation patterns
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
  (require rackunit)
  (check-equal? (string->hashpair "2'2") '("'" (2 2)))
  (check-equal? (string->hashpair ".â4") '(".â" (0 0 4)))
  (check-equal? (string->hashpair ".ý4") '(".ý" (0 0 4)))
  (check-equal? (string->hashpair "'ý4") '("'ý" (0 0 4)))
  (check-equal? (string->hashpair "’ý4") '("’ý" (0 0 4)))
  (check-equal? (string->hashpair "4ý-") '("ý-" (4 0 0)))
  (check-equal? (string->hashpair ".ach4") '(".ach" (0 0 0 0 4))))


(define (apply-map-max patterns)
  ;; each pattern is a list of numbers
  ;; all the patterns have the same length
  (cond
    [(empty? patterns) empty] ; special case
    [(= 1 (length patterns)) (car patterns)] ; no competition for max
    [else (apply map (λ xs (apply max xs)) patterns)])) ; run max against parallel elements


(define (make-points word)
  ;; walk through all the substrings and see if there's a matching pattern.
  ;; if so, pad it out to full length (so we can (apply map max ...) later on)
  (define word-with-dots (format ".~a." (string-downcase word)))
  (define matching-patterns 
    (cond
      [(or (hash-ref (current-word-cache) word-with-dots #f)
           (hash-ref (current-exceptions) word-with-dots #f)) => list]
      [else
       (define word-as-list (string->list word-with-dots))
       (define word-as-list-length (length word-as-list))
       ;; ensures there's at least one (null) element in return value
       (define starting-value (make-list (add1 word-as-list-length) 0))
       (reverse (for*/fold ([acc (cons starting-value null)])
                           ([len (in-range word-as-list-length)] 
                            [index (in-range (- word-as-list-length len))])
                  (define substr (list->string (take (drop word-as-list index) (add1 len))))
                  (cond
                    [(hash-has-key? (current-patterns) substr)
                     (define value (hash-ref (current-patterns) substr))
                     ;; put together head padding + value + tail padding
                     (define pattern-to-add
                       (append (make-list index 0) value (make-list (- (add1 word-as-list-length) (length value) index) 0)))
                     (cons pattern-to-add acc)]
                    [else acc])))])) 
  
  (define max-value-pattern (apply-map-max matching-patterns))
  (cache-word (cons word-with-dots max-value-pattern))
  
  ;; for point list,
  ;; drop first two elements because they represent hyphenation weight
  ;; before the starting "." and between "." and the first letter.
  ;; drop last element because it represents hyphen after last "."
  ;; after you drop these two, then each number corresponds to
  ;; whether a hyphen goes after that letter.
  (drop-right (drop max-value-pattern 2) 1))


;; Find hyphenation points in a word. This is not quite synonymous with syllables.
(define (word->hyphenation-points word 
                                  [min-length default-min-length] 
                                  [min-left-length default-min-left-length] 
                                  [min-right-length default-min-right-length])
  #;((string?) ((or/c #f exact-nonnegative-integer?)(or/c #f exact-nonnegative-integer?)(or/c #f exact-nonnegative-integer?)) . ->* . (listof string?))
  (define (add-no-hyphen-zone points)
    ;; points is a list corresponding to the letters of the word.
    ;; to create a no-hyphenation zone of length n, zero out the first n-1 points
    ;; and the last n points (because the last value in points is always superfluous)
    (let* ([min-left-length (min (or min-left-length default-min-left-length) (length points))]
           [min-right-length (min (or min-right-length default-min-right-length) (length points))])
      (define points-with-zeroes-on-left 
        (append (make-list (sub1 min-left-length) 0) (drop points (sub1 min-left-length))))
      (define points-with-zeroes-on-left-and-right 
        (append (drop-right points-with-zeroes-on-left min-right-length) (make-list min-right-length 0)))
      points-with-zeroes-on-left-and-right))
  (define (make-pieces word)
    (define-values (word-pieces last-piece) 
      (for/fold ([word-pieces empty]
                 [current-piece empty]) 
                ([str (in-list (regexp-match* #rx"." word))] ; explodes word into list of one-character strings (char list is slower)
                 [point (in-list (add-no-hyphen-zone (make-points word)))])
        (define updated-current-piece (cons str current-piece))
        (if (even? point)
            (values word-pieces updated-current-piece) ; even point denotes character
            (values (cons (string-join (reverse updated-current-piece) "") word-pieces) empty)))) ; odd point denotes char + syllable
    (reverse (cons (string-join (reverse last-piece) "") word-pieces)))
  (if (and min-length (< (string-length word) min-length))
      (list word)  
      (make-pieces word)))

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
  (define (insert-hyphens text)
    (regexp-replace* word-pattern text replacer))  
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
  (define (remove-hyphens text)
    (regexp-replace* word-pattern text replacer))
  
  (apply-proc remove-hyphens x omit-string? omit-txexpr?))  


#;(module+ main
    (report (current-word-cache))
    (hyphenate "snowman" "-")
    (parameterize ([current-word-cache (make-hash)]
                   [current-exceptions '("snow-man")])
      ;(reset-patterns)
      (report (current-patterns))
      (hyphenate "snowman" "-"))
    (report (current-word-cache))
    (hyphenate "snowman" "-" )
    #;(define t "supercalifragilisticexpialidocious") 
    #;(hyphenate t "-"))