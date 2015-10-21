#lang racket/base

(module reader racket/base
  (require racket/port syntax/strip-context)
  (provide (rename-out [exception-prep-read read]
                       [exception-prep-read-syntax read-syntax]))
  
  (define (exception-prep-read in)
    (syntax->datum
     (exception-prep-read-syntax #f in)))
  
  (define (exception-prep-read-syntax src in)
    (with-syntax ([str (port->string in)])
      (strip-context
       #'(module anything racket/base
           (require racket/string racket/list hyphenate/private/hyphenate)
           (provide exceptions)
           (define exceptions (apply hash (append-map convert-exception-word (string-split str)))))))))