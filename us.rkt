#lang racket
(require (prefix-in core: "private/hyphenate.rkt") "private/params.rkt")
(provide hyphenate (all-from-out "private/params.rkt"))

(define us-cache (make-hash))
(define us-patterns '#hash(("achin" . (0 0 0 3 0)))) 
(define us-exceptions '("snow-man"))

(define (hyphenate x)
  (parameterize ([current-word-cache us-cache]
                 [current-patterns us-patterns]
                 [current-exceptions us-exceptions])
    (core:hyphenate x)))

