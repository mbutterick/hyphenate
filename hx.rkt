#lang racket/base
(require "main.rkt")
(require xml)
(require tagged-xexpr)


#|
The following grammar describes expressions that create X-expressions:

  xexpr	 	=	 	string
 	 	|	 	(list symbol (list (list symbol string) ...) xexpr ...)
 	 	|	 	(cons symbol (list xexpr ...))
 	 	|	 	symbol
 	 	|	 	valid-char?
 	 	|	 	cdata
 	 	|	 	misc

|#


;; recursively hyphenate strings within xexpr
;; todo: add exclusion #:only [only-proc (λ(x) x)]
(define (hx x)
  (cond 
    [(string? x) (hyphenate x)]
    [(tagged-xexpr? x) x]
    ;;
    ;;
    [else x] ;; catches symbols, valid-chars, and cdata
    )
  
  )


#|
  (define exclusions '(style script)) ; omit these from ever being hyphenated


(cond
    ; todo: the only-proc semantics are illogical.
    ; main issue: keep it out of tags like <style> that parse as textual elements, but are not.
    ; So two choices, opt-out or opt-in.
    ; Problem with opt-out: is set of outlier tags like <style> well-defined?
    ; Won't it make hyphenation naturally overinclusive?
    ; Problem with opt-in: conceals a lot of tags that naturally live inside other tags
    ; only reaches text at the "root level" of the tag.
    [(tagged-xexpr? x) (if (and (only-proc x) (not ((car x) . in? . exclusions)))
                           (map-xexpr-elements hyphenate x)
                           (map-xexpr-elements (λ(x) (if (tagged-xexpr? x) (hyphenate x) x)) x))] ; only process subxexprs
    
    [(string? x) (hyphenate-string x)]
    [else x])
|#

;; how to make cdata:
;;  (make-cdata #f #f "<![CDATA[foobar]]>")