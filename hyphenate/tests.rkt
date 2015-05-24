#lang racket/base
(require (for-syntax racket/base racket/syntax))

(define-syntax (eval-as-untyped stx)
  (syntax-case stx ()
    [(_ exprs ...)
     (with-syntax ([sym (generate-temporary)]) 
       #'(begin
           (module sym typed/racket/base/no-check
             (require rackunit "main.rkt" txexpr)
             exprs ...)
           (require 'sym)))]))

(define-syntax (eval-as-untyped-safe stx)
  (syntax-case stx ()
    [(_ exprs ...)
     (with-syntax ([sym (generate-temporary)]) 
       #'(begin
           (module sym typed/racket/base/no-check
             (require rackunit (submod "main.rkt" safe) txexpr)
             exprs ...)
           (require 'sym)))]))

(define-syntax (eval-as-typed stx)
  (syntax-case stx ()
    [(_ exprs ...)
     (with-syntax ([sym (generate-temporary)]) 
       #'(begin
           (module sym typed/racket
             (require typed/rackunit "../typed/hyphenate/main.rkt" typed/txexpr)
             exprs ...)
           (require 'sym)))]))

(define-syntax-rule (eval-as-typed-and-untyped exprs ...)
  (begin
    (eval-as-untyped exprs ...)
    (eval-as-untyped-safe exprs ...)
    (eval-as-typed exprs ...)))

(eval-as-typed-and-untyped
 (reset-patterns)
 (define omit-em-tag (λ:([x : Txexpr]) (member (car x) '(em))))
 (define omit-p-tag (λ:([x : Txexpr]) (member (car x) '(p))))
 (define omit-foo-zam-tag (λ:([x : Txexpr]) (member (car x) '(foo zam))))
 (define ends-with-s (λ:([x : String]) (regexp-match #rx"s$" x)))
 (define omit-script-tag (λ:([x : Txexpr]) (member (car x) '(script))))
 (define tx-with-attr (λ:([x : Txexpr]) (with-handlers ([exn:fail? (λ(exn) #f)]) 
                                          (equal? (attr-ref x 'hyphens) "no-thanks"))))
 
 (check-equal? (hyphenate "edges") "edges") ;; word without matching patterns
 (check-equal? (hyphenate "polymorphism") "poly\u00ADmor\u00ADphism")
 (check-equal? (hyphenate "POLYmorPHISM") "POLY\u00ADmor\u00ADPHISM")
 (check-equal? (hyphenate "polymorphism" #:min-length 100) "polymorphism")
 (check-equal? (hyphenate "ugly" #:min-length 1) "ug\u00ADly")
 (check-equal? (unhyphenate "poly\u00ADmor\u00ADphism") "polymorphism")
 (check-equal? (hyphenate "polymorphism" #\-) "poly-mor-phism")
 (check-equal? (hyphenate "polymorphism" "foo") "polyfoomorfoophism")
 (check-equal? (unhyphenate "polyfoomorfoophism" "foo") "polymorphism")
 (check-equal? (hyphenate "circular polymorphism squandering") "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism squan\u00ADder\u00ADing")
 (check-equal? (hyphenate '(p "circular polymorphism" amp (em "squandering"))) '(p "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism" amp (em "squan\u00ADder\u00ADing")))
 (check-equal? (hyphenate "present project") "present project") ; exception words
 ;; test these last so exceptions have been set up already
 
 (check-equal? (word->hyphenation-points "polymorphism") '("poly" "mor" "phism"))
 (check-equal? (word->hyphenation-points "present") '("present")) ; exception word
 
 (check-true (exception-word? "Foobar"))
 (check-true (exception-word? "foobar"))
 (check-false (exception-word? "foobar!"))
 (check-true (exception-word? "foo-bar"))
 (check-false (exception-word? "foo bar"))
 
 ;; omit certain tags
 (check-equal? (hyphenate '(p "circular polymorphism" amp (em "squandering")) #:omit-txexpr omit-em-tag) 
               '(p "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism" amp (em "squandering")))
 
 (check-equal? (hyphenate '(p "circular polymorphism" amp (em "squandering")) #:omit-txexpr omit-p-tag) 
               '(p "circular polymorphism" amp (em "squandering")))
 
 (check-equal? (hyphenate '(p  (foo "circular") (bar "circular") (zam "circular")) #:omit-txexpr omit-foo-zam-tag) 
               '(p  (foo "circular") (bar "cir\u00ADcu\u00ADlar") (zam "circular")))
 
 ; omit txexprs with an attribute
 (check-equal? (hyphenate '(p  (foo ((hyphens "no-thanks")) "circular") (foo "circular")) 
                          #:omit-txexpr tx-with-attr) 
               '(p  (foo ((hyphens "no-thanks")) "circular") (foo "cir\u00ADcu\u00ADlar")))
 
 
 ;; omit strings that end with "s"
 (check-equal? (hyphenate '(p (foo "curses tailfeathers") (foo "curses tailfeather")) #:omit-string ends-with-s)
               '(p (foo "curses tailfeathers") (foo "curs\u00ADes tail\u00ADfeath\u00ADer")))
 
 ;; omit words that end with "s"
 (check-equal? (hyphenate '(p (foo "curses tailfeathers") (foo "curses tailfeather")) #:omit-word ends-with-s)
               '(p (foo "curses tailfeathers") (foo "curses tail\u00ADfeath\u00ADer")))
 
 
 (check-equal? (unhyphenate '(p (script "tail-feathers") (em "tail-feathers")) #\- #:omit-txexpr omit-script-tag) 
               '(p (script "tail-feathers") (em "tailfeathers")))
 
 (check-equal? (unhyphenate '(p "cir-cu-lar poly-mor-phism" "cir-cu-lar poly-mor-phisms") #\- #:omit-string ends-with-s) 
               '(p "circular polymorphism" "cir-cu-lar poly-mor-phisms"))
 
 (check-equal? (hyphenate "polymorphism" #\- #:min-left-length 5 #:min-right-length 5) "polymor-phism")
 (check-equal? (hyphenate "polymorphism" #\- #:min-left-length 3 #:min-right-length 7) "poly-morphism")
 (check-equal? (hyphenate "polymorphism" #\- #:min-left-length 7 #:min-right-length 7) "polymorphism")
 (check-equal? (hyphenate "polymorphism" #\* #:exceptions '("polymo-rphism")) "polymo*rphism"))