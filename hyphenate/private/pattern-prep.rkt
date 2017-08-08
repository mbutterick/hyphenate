#lang racket/base
(require hyphenate/private/core racket/list racket/file)
(provide (rename-out [pmb #%module-begin]) #%app #%datum #%top-interaction)

(define-syntax-rule (pmb SRC STRS)
  (#%module-begin     
   (provide patterns)
   (define strs (list . STRS))
   (define src SRC)
   (define-values (dir name _) (split-path src))
   (define pattern-cache-path (build-path dir "compiled" "pattern-cache.rktd"))
   (unless (and (file-exists? pattern-cache-path)
                (> (file-or-directory-modify-seconds pattern-cache-path)
                   (file-or-directory-modify-seconds src)))
     (define patterns (for/hasheq ([str (in-list strs)])
                                  (define strkey+val (string->hashpair str))
                                  (values (string->symbol (first strkey+val)) (second strkey+val))))
     (write-to-file patterns pattern-cache-path #:exists 'replace))
   (define patterns (file->value pattern-cache-path))))

(module+ reader
  (provide read-syntax)
  
  (define (read-syntax src in)
    (with-syntax ([SRC (path->string src)]
                  [STRS (for/list ([line (in-lines in)]
                                   #:when (and (positive? (string-length line)) ; omit empty
                                               (not (regexp-match #rx"^;" line)))) ; omit comments
                                  line)])
      (syntax->datum #'(module patterns hyphenate/private/pattern-prep
                         SRC STRS)))))