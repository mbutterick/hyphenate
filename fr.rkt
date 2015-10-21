#lang racket
(require (prefix-in core: "private/hyphenate.rkt") "private/params.rkt" "fr/patterns.rkt" "us/exceptions.rkt")
(provide hyphenate unhyphenate (all-from-out "private/params.rkt"))

(define (hyphenate x)
  (parameterize ([current-word-cache (make-hash)]
                 [current-patterns patterns]
                 [current-exceptions exceptions])
    (core:hyphenate x)))

(define unhyphenate core:unhyphenate)