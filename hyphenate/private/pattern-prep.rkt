#lang racket/base
(require hyphenate/private/core racket/list racket/file)
(provide (rename-out [pmb #%module-begin]) #%app #%datum #%top-interaction)

(define-syntax-rule (pmb STRS)
  (#%module-begin     
   (provide patterns)
   (define patterns (for/hasheq ([str (in-list (list . STRS))])
                      (define key+val (string->hashpair str))
                      (values (string->symbol (first key+val)) (second key+val))))
   (module+ main patterns)))

(module+ reader
  (provide read-syntax)
  (define (read-syntax src in)
    (with-syntax ([STRS (for/list ([line (in-lines in)]
                                   #:when (and (positive? (string-length line)) ; omit empty
                                               (not (regexp-match #rx"^;" line)))) ; omit comments
                          line)])
      (syntax->datum #'(module patterns hyphenate/private/pattern-prep
                         STRS)))))