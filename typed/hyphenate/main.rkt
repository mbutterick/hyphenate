#lang typed/racket/base
(require sugar/include)
(include-without-lang-line "main-helper.rkt")
(require typed/sugar/define racket/string racket/list racket/bool)
(require "patterns-hashed.rkt" "exceptions.rkt" "core-predicates.rkt")
(provide hyphenate unhyphenate reset-patterns word->hyphenation-points exception-word? exception-words?)

;; module data, define now but set! them later (because they're potentially big & slow)
(define: patterns : Pattern-Hash (make-hash))
(define: pattern-cache : Pattern-Hash (make-hash))

;; module default values
(define: default-min-length : Natural 5)
(define: default-min-left-length : Natural 2)
(define: default-min-right-length : Natural 2)
(define: default-joiner : Char #\u00AD)

(define/typed (add-pattern-to-cache pat)
  (Pattern-Hash-Pair -> Void)
  (hash-set! pattern-cache (car pat) (cdr pat)))

;; Convert the hyphenated pattern into a point array for use later.
(define/typed (add-exception exception)
  (Pattern -> Void)
  (define/typed (make-key x)
    (Pattern -> Pattern-Hash-Key)
    (format ".~a." (string-replace x "-" "")))
  (define/typed (make-value x)
    (Pattern -> Pattern-Hash-Value)
    `(0 ,@(map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"[a-z]" x)) 0))
  (add-pattern-to-cache (cons (make-key exception) (make-value exception)))
  (void))

(define-syntax-rule (hash-empty? h) (zero? (hash-count h)))

(define/typed (initialize-patterns)
  (-> Void)
  (when (hash-empty? pattern-cache)
    (for-each add-exception default-exceptions))
  (when (hash-empty? patterns)
    (set! patterns hashed-patterns)))

(define/typed (reset-patterns)
  (-> Void)
  (define: blank : Pattern-Hash (make-hash))
  (set! pattern-cache (hash-copy blank))
  (set! patterns (hash-copy blank))
  (initialize-patterns))

;; An exception-word is a string of word characters or hyphens.
(define/typed (exception-word? x)
  (Any -> Boolean)
  (and (string? x) (regexp-match #px"^[\\w-]+$" x) #t))


(define/typed (exception-words? xs) 
  (Any -> Boolean)
  (and (list? xs) (andmap exception-word? xs)))


(define/typed (string->natural i)
  (String -> (Option Natural))
  (let* ([result (string->number i)]
         [result (and (number? result) (inexact->exact result))]
         [result (and (exact-nonnegative-integer? result) result)])
    result))

(define/typed (string->hashpair pat)
  (String -> Pattern-Hash-Pair)
  
  (define boundary-name ".")
  
  ;; first convert the pattern to a list of alternating letters and numbers.
  ;; insert zeroes where there isn't a number in the pattern.
  (define new-pat
    (let*: ([pat : (Listof String) (regexp-match* #rx"." pat)] ; convert to list
            [pat : (Listof (U String Natural)) ((inst map (U String Natural) String) (λ(i) (or (string->natural i) i)) pat)] ; convert numbers
            [pat : (Listof (U String Natural)) (if (string? (car pat)) (cons 0 pat) pat)] ; add zeroes to front where needed
            [pat : (Listof (U String Natural)) (if (string? (car (reverse pat))) (reverse (cons 0 (reverse pat))) pat)]) ; and back
      (apply append
             (reverse (for/fold: ([acc : (Listof (Listof (U String Natural))) null]) 
                        ([current (in-list pat)][i (in-naturals)])
                        (if (= i (sub1 (length pat))) 
                            (cons (reverse (list current)) acc)
                            (let ([next (list-ref pat (add1 i))])
                              ;; insert zeroes where there isn't a number
                              (cons (reverse (if (and (or (equal? current boundary-name) (string? current)) (string? next)) 
                                                 (list current 0)
                                                 (list current))) acc))))))))
  
  ;; then slice out the string & numerical parts to be a key / value pair.
  (define value (filter exact-nonnegative-integer? new-pat))
  (define key (filter string? new-pat))
  (cons (apply string-append key) value))

(define/typed (make-points word)
  (String -> Pattern-Hash-Value)
  ;; walk through all the substrings and see if there's a matching pattern.
  ;; if so, pad it out to full length (so we can (apply map max ...) later on)
  (define: word-with-dots : String (format ".~a." (string-downcase word)))
  (define: matching-patterns : (Listof Pattern-Hash-Value) 
    (cond
      [(hash-has-key? pattern-cache word-with-dots) (list (hash-ref pattern-cache word-with-dots))]
      [else
       (let ([word-as-list (string->list word-with-dots)])
         ;; ensures there's at least one (null) element in return value
         (define starting-value (make-list (add1 (length word-as-list)) 0))
         (reverse (for*/fold: ([acc : (Listof Pattern-Hash-Value) (cons starting-value null)])
                    ([len (in-range (length word-as-list))] 
                     [index (in-range (- (length word-as-list) len))])
                    (define substring (list->string (take (drop word-as-list index) (add1 len))))
                    (cond
                      [(hash-has-key? patterns substring)
                       (define value (hash-ref patterns substring))
                       ;; put together head padding + value + tail padding
                       (define pattern-to-add (append (make-list index 0) value (make-list (- (add1 (length word-as-list)) (length value) index) 0)))
                       (cons pattern-to-add acc)]
                      [else acc]))))])) 
  
  (define/typed (apply-map-max xss)
    ((Listof Pattern-Hash-Value) -> Pattern-Hash-Value)
    (if (ormap empty? (list xss (car xss))) 
        empty
        (cons (apply max ((inst map Natural Pattern-Hash-Value) car xss))
              (apply-map-max ((inst map Pattern-Hash-Value Pattern-Hash-Value) cdr xss)))))
  
  (define: max-value-pattern : Pattern-Hash-Value (apply-map-max matching-patterns))
  (add-pattern-to-cache (cons word-with-dots max-value-pattern))
  
  ;; for point list,
  ;; drop first two elements because they represent hyphenation weight
  ;; before the starting "." and between "." and the first letter.
  ;; drop last element because it represents hyphen after last "."
  ;; after you drop these two, then each number corresponds to
  ;; whether a hyphen goes after that letter.
  (drop-right (drop max-value-pattern 2) 1))


;; Find hyphenation points in a word. This is not quite synonymous with syllables.
(define/typed (word->hyphenation-points word 
                                        [min-length default-min-length] 
                                        [min-left-length default-min-left-length] 
                                        [min-right-length default-min-right-length])
  (case-> (String -> (Listof String))
          (String (Option Natural) -> (Listof String))
          (String (Option Natural)(Option Natural) -> (Listof String))
          (String (Option Natural)(Option Natural)(Option Natural) -> (Listof String)))
  (define/typed (add-no-hyphen-zone points)
    ((Listof Natural) -> (Listof Natural))
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
  (define/typed (make-pieces word)
    (String -> (Listof String))
    (define-values (word-pieces last-piece) 
      (for/fold: ([word-pieces : (Listof String) empty]
                  [current-piece : (Listof String) empty]) 
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
(define/typed (joiner->string joiner)
  ((U Char String) -> String)
  (format "~a" joiner))

(define/typed (apply-proc proc x [omit-string (λ(x) #f)] [omit-txexpr (λ(x) #f)])
  (case->
   ((String -> String) Xexpr -> Xexpr)
   ((String -> String) Xexpr (String -> Any) -> Xexpr)
   ((String -> String) Xexpr (String -> Any) (Txexpr -> Any) -> Xexpr))
  (let loop ([x x])
    (cond
      [(and (string? x) (not (omit-string x))) (proc x)]
      [(and (txexpr? x) (not (omit-txexpr x))) 
       (make-txexpr (get-tag x) (get-attrs x) ((inst map Txexpr-Element Txexpr-Element) loop (get-elements x)))]
      [else x])))


(define/typed (hyphenate x [joiner default-joiner] 
                         #:exceptions [extra-exceptions empty]  
                         #:min-length [min-length default-min-length]
                         #:min-left-length [min-left-length default-min-left-length]
                         #:min-right-length [min-right-length default-min-right-length]
                         #:omit-word [omit-word? (λ(x) #f)]
                         #:omit-string [omit-string? (λ(x) #f)]
                         #:omit-txexpr [omit-txexpr? (λ(x) #f)])
  (case->
   (Xexpr 
    [#:exceptions (Listof String)]
    [#:min-length (Option Natural)]
    [#:min-left-length (Option Natural)]
    [#:min-right-length (Option Natural)]
    [#:omit-word (String -> Any)]
    [#:omit-string (String -> Any)]
    [#:omit-txexpr (Txexpr -> Any)] -> Xexpr)
   (Xexpr (U Char String) 
          [#:exceptions (Listof String)]
          [#:min-length (Option Natural)]
          [#:min-left-length (Option Natural)]
          [#:min-right-length (Option Natural)]
          [#:omit-word (String -> Any)]
          [#:omit-string (String -> Any)]
          [#:omit-txexpr (Txexpr -> Any)] -> Xexpr))
  (initialize-patterns) ; reset everything each time hyphenate is called
  (for-each add-exception extra-exceptions)
  ;; todo?: connect this regexp pattern to the one used in word? predicate
  (define word-pattern #px"\\w+") ;; more restrictive than exception-word
  (define/typed (replacer word . words)
    (String String * -> String)
    (if (not (omit-word? word)) 
        (string-join (word->hyphenation-points word min-length min-left-length min-right-length) (joiner->string joiner)) 
        word))
  (define/typed (insert-hyphens text)
    (String -> String)
    (regexp-replace* word-pattern text replacer))  
  (apply-proc insert-hyphens x omit-string? omit-txexpr?))


(define/typed (unhyphenate x [joiner default-joiner] 
                           #:omit-word [omit-word? (λ(x) #f)]
                           #:omit-string [omit-string? (λ(x) #f)]
                           #:omit-txexpr [omit-txexpr? (λ(x) #f)])
  (case->
   (Xexpr 
    [#:omit-word (String -> Any)]
    [#:omit-string (String -> Any)]
    [#:omit-txexpr (Txexpr -> Any)] -> Xexpr)
   (Xexpr (U Char String) 
          [#:omit-word (String -> Any)]
          [#:omit-string (String -> Any)]
          [#:omit-txexpr (Txexpr -> Any)] -> Xexpr))
  
  (define word-pattern (pregexp (format "[\\w~a]+" joiner)))
  (define/typed (replacer word . words)
    (String String * -> String)
    (if (not (omit-word? word)) 
        (string-replace word (joiner->string joiner) "")
        word))
  (define/typed (remove-hyphens text)
    (String -> String)
    (regexp-replace* word-pattern text replacer))
  
  (apply-proc remove-hyphens x omit-string? omit-txexpr?))  

(module+ main
  (initialize-patterns)
  (hyphenate "supercalifragilisticexpialidocious" "-")
  #;(define t "supercalifragilisticexpialidocious") 
  #;(hyphenate t "-"))



