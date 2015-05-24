#lang typed/racket/base
(provide (all-defined-out))

(define-type Pattern String)
(define-type Patterns (Listof String))
(define-type Pattern-Hash-Key Pattern)
(define-type Pattern-Hash-Value (Listof Natural))
(define-type Pattern-Hash (HashTable Pattern-Hash-Key Pattern-Hash-Value))
(define-type Pattern-Hash-Pair (Pairof Pattern-Hash-Key Pattern-Hash-Value))
(define-type Exception-Word String)