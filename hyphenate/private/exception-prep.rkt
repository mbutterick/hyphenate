#lang racket/base
(require racket/string racket/list hyphenate/private/core)
(provide (rename-out [emb #%module-begin]) #%app #%datum #%top-interaction)

(define-syntax-rule (emb STRS)
  (#%module-begin
   (provide exceptions)
   (define exceptions (for/hash ([str (in-list (list . STRS))])
                        (define key+val (exception-word->word+pattern str))
                        (values (first key+val) (second key+val))))
   (module+ main exceptions)))

(module+ reader
  (require racket/port)
  (provide (rename-out [exception-prep-read-syntax read-syntax]))
  
  (define (exception-prep-read-syntax src in)
    (with-syntax ([STRS (string-split (port->string in))])
      (syntax->datum
       #'(module exception-prep hyphenate/private/exception-prep
           STRS)))))