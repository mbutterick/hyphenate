#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax (reprovide stx)
  (syntax-case stx () 
    [(_ dir)
     (let ([pathname (format "~a/main.rkt" (syntax->datum #'dir))])
       #`(begin
           (require #,pathname)
           (provide (all-from-out #,pathname))))]))