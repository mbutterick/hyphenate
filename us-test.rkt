#lang racket
(require hyphenate/us rackunit)

(check-equal? (hyphenate "snowman machine") "snow\u00ADman mach\u00ADine")
(check-equal? (hyphenate "snowball freakout") "snowball freakout")