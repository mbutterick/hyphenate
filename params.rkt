#lang racket/base
(provide (all-defined-out))

(define current-patterns (make-parameter (hash)))
(define current-exceptions (make-parameter null))
(define current-word-cache (make-parameter (make-hash)))