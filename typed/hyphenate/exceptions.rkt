#lang typed/racket/base
(require "core-predicates.rkt")
(provide default-exceptions)

; Knuth and Liang's original exception patterns from classic TeX.
; In the public domain.
(define: kl-exceptions : Patterns
  (map symbol->string '(as-so-ciate as-so-ciates dec-li-na-tion oblig-a-tory phil-an-thropic present presents project projects reci-procity re-cog-ni-zance ref-or-ma-tion ret-ri-bu-tion ta-ble)))

(define: mb-exceptions : Patterns
  (map symbol->string '(real-ly law-yer law-yers law-yered law-yer-ing law-yer-ly)))

(define: default-exceptions : Patterns
  (append kl-exceptions mb-exceptions))