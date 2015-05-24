#lang info
(define collection 'multi)
(define deps '("base" "sugar" "txexpr" "typed-racket-lib" "typed-racket-more" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc"))
(define test-omit-paths '("typed/hyphenate/patterns-hashed.rkt"))