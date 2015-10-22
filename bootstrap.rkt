#lang racket/base
(require (for-syntax racket/base syntax/strip-context))
(provide (all-defined-out))

(define-syntax (build-main stx)
  (syntax-case stx () 
    [(_ dir)
     (with-syntax ([patterns-path (datum->syntax stx (format "~a/patterns.rkt" (syntax->datum #'dir)))]
                   [exceptions-path (datum->syntax stx (format "~a/exceptions.rkt" (syntax->datum #'dir)))])
       (replace-context stx
                        #'(begin
                            (require (prefix-in core: hyphenate/core) hyphenate/params patterns-path exceptions-path)
                            (provide hyphenate unhyphenate (all-from-out hyphenate/params))
                            
                            (define hyphenate
                              (make-keyword-procedure (λ (kws kw-args . rest)
                                                        (parameterize ([current-word-cache (make-hash)]
                                                                       [current-patterns patterns]
                                                                       [current-exceptions exceptions])
                                                          (keyword-apply core:hyphenate kws kw-args rest)))))
                            
                            (define unhyphenate core:unhyphenate))))]))