#lang info
(define collection "hyphenate")
(define deps '("base" "sugar" "txexpr" "rackunit-lib"))
(define update-implies '("txexpr" "sugar"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/hyphenate.scrbl" ())))
(define compile-omit-paths '("tests.rkt"))