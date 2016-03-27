#lang info
(define version "0.2")
(define collection 'multi)
(define deps '("base" ["sugar" #:version "0.2"] ["txexpr" #:version "0.2"]))
(define update-implies '("txexpr" "sugar"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("hyphenate/scribblings/hyphenate.scrbl" ())))
(define compile-omit-paths '("hyphenate/tests.rkt"))