#lang racket/base
(require (for-syntax racket/base syntax/strip-context)
         txexpr/base
         sugar/define
         racket/contract
         (only-in xml xexpr/c)
         (prefix-in core: hyphenate/private/core))

(provide build-main)

;; An exception-word is a string of word characters or hyphens.
(define (exception-word? x)
  (and (string? x) (regexp-match #px"^(\\p{L}|-)+$" x) #t))
(define (exception-words? xs) 
  (and (list? xs) (andmap exception-word? xs)))

(define hyphenate/c
  ((xexpr?) ((or/c char? string?) 
             #:exceptions exception-words? 
             #:min-length (or/c integer? #f)
             #:omit-word (string? . -> . any/c)
             #:omit-string (string? . -> . any/c)
             #:omit-txexpr (txexpr? . -> . any/c)
             #:min-left-length (or/c (and/c integer? positive?) #f)
             #:min-right-length (or/c (and/c integer? positive?) #f)) . ->* . xexpr/c))

(define (make-hyphenate-function patterns exceptions)
  (make-keyword-procedure
   ;; put caches here so they can persist across successive invocations of the function.
   ;; but remain distinct between instantiations of this module (say, us vs fr)
   ;; pass them as arguments to the core:hyphenate func
   (let ([word-cache exceptions] ; preload exceptions
         [pattern-cache patterns])
     (λ (kws kw-args . rest)
       (keyword-apply core:hyphenate kws kw-args word-cache pattern-cache rest)))))

(define unhyphenate
  (make-keyword-procedure
   (λ (kws kw-args . rest)
     (keyword-apply core:unhyphenate kws kw-args rest))))

(define unhyphenate/c
  ((xexpr/c) ((or/c char? string?) 
              #:omit-word (string? . -> . any/c)
              #:omit-string (string? . -> . any/c)
              #:omit-txexpr (txexpr? . -> . any/c)) . ->* . xexpr/c))

(define-syntax (build-main stx)
  (syntax-case stx () 
    [(_ dir)
     (with-syntax ([PATTERNS-PATH (path->string (build-path (symbol->string(syntax->datum #'dir)) "patterns.rkt"))]
                   [EXCEPTIONS-PATH (path->string (build-path (symbol->string (syntax->datum #'dir)) "exceptions.rkt"))]
                   [PATTERNS-ID 'patterns]
                   [EXCEPTIONS-ID 'exceptions])
       #'(begin
           (require PATTERNS-PATH EXCEPTIONS-PATH)
           (define+provide+safe hyphenate
             hyphenate/c
             (make-hyphenate-function PATTERNS-ID EXCEPTIONS-ID))
           (provide+safe [unhyphenate unhyphenate/c])))]))