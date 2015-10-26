#lang racket/base

(module reader racket/base
  (require racket/port syntax/strip-context)
  (provide (rename-out [pattern-prep-read read]
                       [pattern-prep-read-syntax read-syntax]))
  
  (define (pattern-prep-read in)
    (syntax->datum (pattern-prep-read-syntax #f in)))
  
  (define (pattern-prep-read-syntax src in)
    (with-syntax ([str (port->string in)])
      (strip-context
       #'(module pattern-prep racket/base
           (require hyphenate/core racket/list racket/string)
           (provide patterns)
           (define patterns (apply hash (append-map string->hashpair (string-split str)))))))))