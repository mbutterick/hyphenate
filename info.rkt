#lang info
(define collection "hyphenate")
(define scribblings '(("scribblings/hyphenate.scrbl" ())))
(define compile-omit-paths '("tests.rkt"))
(define deps '("base" "txexpr"))
(define build-deps '("scribble-lib" "racket-doc"))
