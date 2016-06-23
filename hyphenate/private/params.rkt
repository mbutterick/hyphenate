#lang racket/base
(provide (all-defined-out))

(define current-patterns (make-parameter (make-hash)))
(define current-exceptions (make-parameter (hash)))
(define current-word-cache (make-parameter (make-hash)))