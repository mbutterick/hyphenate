#lang racket/base
(require "main.rkt")

(require rackunit)

(require/expose "main.rkt" (word->hyphenation-points))

(check-equal? (hyphenate "polymorphism") "poly\u00ADmor\u00ADphism")
(check-equal? (hyphenate "polymorphism" #:min-length 100) "polymorphism")
(check-equal? (hyphenate "ugly" #:min-length 1) "ug\u00ADly")
(check-equal? (unhyphenate "poly\u00ADmor\u00ADphism") "polymorphism")
(check-equal? (hyphenatef "polymorphism" (λ(x) #f)) "polymorphism")
(check-equal? (hyphenate "polymorphism" #\-) "poly-mor-phism")
(check-equal? (hyphenate "polymorphism" "foo") "polyfoomorfoophism")
(check-equal? (unhyphenate "polyfoomorfoophism" "foo") "polymorphism")
(check-equal? (hyphenate "polymorphism" #\* #:exceptions '("polymo-rphism")) "polymo*rphism")
(check-equal? (hyphenate "circular polymorphism squandering") "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism squan\u00ADder\u00ADing")
(check-equal? (hyphenate "present project") "present project") ; exception words
;; test these last so exceptions have been set up already
(check-equal? (word->hyphenation-points "polymorphism") '("poly" "mor" "phism"))
(check-equal? (word->hyphenation-points "present") '("present")) ; exception word

