#lang racket/base
(require (for-syntax racket/base syntax/strip-context)
         txexpr/base
         sugar/define
         racket/contract
         racket/file
         racket/fasl
         (only-in xml xexpr/c)
         (prefix-in core: hyphenate/private/core))

(provide (rename-out [mb #%module-begin]) #%top-interaction #%top #%datum #%app)

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
             #:min-right-length (or/c (and/c integer? positive?) #f)
             #:min-hyphens (and/c integer? positive?)) . ->* . xexpr/c))

(define (make-hyphenate-function patterns exceptions)
  (make-keyword-procedure
   ;; put caches here so they can persist across successive invocations of the function.
   ;; but remain distinct between instantiations of this module (say, us vs fr)
   ;; pass them as arguments to the core:hyphenate func
   (let ([word-cache exceptions] ; preload exceptions
         [pattern-cache patterns])
     (λ (kws kw-args . rest)
       (keyword-apply core:hyphenate kws kw-args word-cache pattern-cache rest)))))

(define (make-unhyphenate-function)
  (make-keyword-procedure
   (λ (kws kw-args . rest)
     (keyword-apply core:unhyphenate kws kw-args rest))))

(define unhyphenate/c
  ((xexpr/c) ((or/c char? string?) 
              #:omit-word (string? . -> . any/c)
              #:omit-string (string? . -> . any/c)
              #:omit-txexpr (txexpr? . -> . any/c)) . ->* . xexpr/c))

(define (load-from-cache-if-possible module-path cache-path id-sym)
  (unless (and (file-exists? cache-path)
               (> (file-or-directory-modify-seconds cache-path)
                  (file-or-directory-modify-seconds module-path)))
    (with-output-to-file cache-path
      (λ () (s-exp->fasl (dynamic-require module-path id-sym) (current-output-port)))
      #:exists 'replace))
  (fasl->s-exp (open-input-file cache-path)))

(define-syntax (mb stx)
  (syntax-case stx () 
    [(_ DIR)
     (let* ([base (let-values ([(base name _) (split-path (syntax-source stx))])
                    base)]
            [dir (build-path base (symbol->string (syntax->datum #'DIR)))])
       (with-syntax ([PATTERNS-PATH (build-path dir "patterns.rkt")]
                     [PATTERN-CACHE-PATH (build-path dir "compiled" "patterns-cache.rktd")]
                     [EXCEPTIONS-PATH  (build-path dir "exceptions.rkt")]
                     [EXCEPTIONS-CACHE-PATH (build-path dir "compiled" "exceptions-cache.rktd")]
                     [PATTERNS-ID 'patterns]
                     [EXCEPTIONS-ID 'exceptions])
         #'(#%module-begin
            (define PATTERNS-ID (load-from-cache-if-possible PATTERNS-PATH PATTERN-CACHE-PATH 'PATTERNS-ID))
            ;; a file-cached hash is immutable, so convert it
            (define EXCEPTIONS-ID (make-hash (hash->list (load-from-cache-if-possible EXCEPTIONS-PATH EXCEPTIONS-CACHE-PATH 'EXCEPTIONS-ID))))
            (define+provide (post-installer home-dir)
              (load-from-cache-if-possible PATTERNS-PATH PATTERN-CACHE-PATH 'PATTERNS-ID)
              (load-from-cache-if-possible EXCEPTIONS-PATH EXCEPTIONS-CACHE-PATH 'EXCEPTIONS-ID))
            (define+provide+safe hyphenate
              hyphenate/c
              (make-hyphenate-function PATTERNS-ID EXCEPTIONS-ID))
            (define+provide+safe unhyphenate
              unhyphenate/c
              (make-unhyphenate-function)))))]))