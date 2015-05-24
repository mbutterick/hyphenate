#lang racket/base
(require sugar/define txexpr (only-in xml xexpr/c))
(require-via-wormhole "../typed/hyphenate/main.rkt")

(provide+safe
 [hyphenate ((xexpr?) 
             ((or/c char? string?) 
              #:exceptions exception-words? 
              #:min-length (or/c integer? #f)
              #:omit-word (string? . -> . any/c)
              #:omit-string (string? . -> . any/c)
              #:omit-txexpr (txexpr? . -> . any/c)
              #:min-left-length (or/c (and/c integer? positive?) #f)
              #:min-right-length (or/c (and/c integer? positive?) #f)) . ->* . xexpr/c)]
 [unhyphenate ((xexpr/c) 
               ((or/c char? string?) 
                #:omit-word (string? . -> . any/c)
                #:omit-string (string? . -> . any/c)
                #:omit-txexpr (txexpr? . -> . any/c)) . ->* . xexpr/c)]
 reset-patterns
 [word->hyphenation-points ((string?) ((or/c #f exact-nonnegative-integer?)(or/c #f exact-nonnegative-integer?)(or/c #f exact-nonnegative-integer?)) . ->* . (listof string?))]
 [exception-word? (string? . -> . boolean?)])