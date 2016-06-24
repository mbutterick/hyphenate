#lang racket/base
(require (for-syntax racket/base syntax/strip-context))
(provide build-main)

(define-syntax (build-main stx)
  (syntax-case stx () 
    [(_ dir)
     (with-syntax ([patterns-path (format "~a/patterns.rkt" (syntax->datum #'dir))]
                   [exceptions-path (format "~a/exceptions.rkt" (syntax->datum #'dir))])
       (replace-context
        stx
        #'(begin
            (require txexpr/base
                     sugar/define
                     (only-in xml xexpr/c)
                     (prefix-in core: hyphenate/private/core)
                     hyphenate/private/params
                     patterns-path exceptions-path)
            (provide (all-from-out hyphenate/private/params))
            
            (module+ safe
              ;; An exception-word is a string of word characters or hyphens.
              (define (exception-word? x)
                (and (string? x) (regexp-match #px"^(\\p{L}|-)+$" x) #t))
              (define (exception-words? xs) 
                (and (list? xs) (andmap exception-word? xs))))
            
            (define+provide+safe hyphenate
              ((xexpr?) ((or/c char? string?) 
                         #:exceptions exception-words? 
                         #:min-length (or/c integer? #f)
                         #:omit-word (string? . -> . any/c)
                         #:omit-string (string? . -> . any/c)
                         #:omit-txexpr (txexpr? . -> . any/c)
                         #:min-left-length (or/c (and/c integer? positive?) #f)
                         #:min-right-length (or/c (and/c integer? positive?) #f)) . ->* . xexpr/c)
              (make-keyword-procedure
               (λ (kws kw-args . rest)
                 (parameterize ([current-word-cache (make-hash (map (λ(xs) (apply cons xs)) exceptions))]
                                [current-patterns patterns])
                   (keyword-apply core:hyphenate kws kw-args rest)))))
            
            (define+provide+safe unhyphenate
              ((xexpr/c) ((or/c char? string?) 
                          #:omit-word (string? . -> . any/c)
                          #:omit-string (string? . -> . any/c)
                          #:omit-txexpr (txexpr? . -> . any/c)) . ->* . xexpr/c)
              (make-keyword-procedure (λ (kws kw-args . rest)
                                        (keyword-apply core:unhyphenate kws kw-args rest)))))))]))