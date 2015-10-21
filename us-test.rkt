#lang racket
(require hyphenate/us rackunit)

(check-equal? (hyphenate "lawyer machine") "law\u00ADyer ma\u00ADchine")
(check-equal? (hyphenate "snowball freakout") "snow\u00ADball freak\u00ADout")
(check-equal? (hyphenate "formidable") "for\u00ADmi\u00ADda\u00ADble")