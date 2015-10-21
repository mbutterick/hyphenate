#lang racket
(require (prefix-in core: hyphenate/core) hyphenate/params "patterns.rkt" "exceptions.rkt")
(provide hyphenate unhyphenate (all-from-out hyphenate/params))

(define (hyphenate x)
  (parameterize ([current-word-cache (make-hash)]
                 [current-patterns patterns]
                 [current-exceptions exceptions])
    (core:hyphenate x)))

(define unhyphenate core:unhyphenate)