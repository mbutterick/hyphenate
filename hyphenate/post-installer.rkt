#lang racket/base
(provide (all-defined-out))

(define (post-installer home-dir)
  (for ([mod '(hyphenate/us hyphenate/fr)])
    (displayln (format "running post-installer in ~a" mod))
    (define proc (dynamic-require mod 'post-installer))
    (proc home-dir)))