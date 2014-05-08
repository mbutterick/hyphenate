#lang scribble/manual

@(require scribble/eval (for-label txexpr racket hyphenate xml))

@(define my-eval (make-base-eval))
@(my-eval `(require (submod txexpr safe) (submod hyphenate safe) xml))


@title{Hyphenate}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]


@defmodule[#:multi (hyphenate (submod hyphenate safe))]

A simple hyphenation engine that uses the Knuth–Liang hyphenation algorithm originally developed for TeX. This implementation is a port of Ned Batchelder's @link["http://nedbatchelder.com/code/modules/hyphenate.html"]{Python version}. I have added little to their work. Accordingly, I take little credit.

I originally put together this module to handle hyphenation for my web-based book @link["http://practicaltypography.com"]{Butterick's Practical Typography} (which I made with Racket & Scribble). Though support for CSS-based hyphenation in web browsers is @link["http://caniuse.com/#search=hyphen"]{still iffy}, soft hyphens work reliably well. But putting them into the text manually is a drag. Thus a module was born.

@section{Installation}

At the command line:
@verbatim{raco pkg install hyphenate}

After that, you can update the package like so:
@verbatim{raco pkg update hyphenate}

@section{Importing the module}


The module operates in two modes: fast and safe. Fast mode is the default, which you get by importing the module in the usual way: @code{(require hyphenate)}. 

Safe mode enables the function contracts documented below. Use safe mode by importing the module as @code{(require (submod hyphenate safe))}.

@section{Interface}

@defproc[
(hyphenate 
[xexpr xexpr/c] 
[joiner (or/c char? string?) (integer->char #x00AD)]
[#:exceptions exceptions (listof string?) empty]
[#:min-length length (or/c integer? false?) 5]
[#:omit-word word-test (string? . -> . any/c) (λ(x) #f)]
[#:omit-string string-test (string? . -> . any/c) (λ(x) #f)]
[#:omit-txexpr txexpr-test (txexpr? . -> . any/c) (λ(x) #f)])
xexpr/c]
Hyphenate @racket[_xexpr] by calculating hyphenation points and inserting @racket[_joiner] at those points. By default, @racket[_joiner] is the soft hyphen (Unicode 00AD = decimal 173). Words shorter than @racket[#:min-length] @racket[_length] will not be hyphenated. To hyphenate words of any length, use @racket[#:min-length] @racket[#f].

@margin-note{The REPL displays a soft hyphen as @code{\u00AD}. But in ordinary use, you'll only see a soft hyphen when it appears at the end of a line or page as part of a hyphenated word. Otherwise it's not displayed. In most of the examples here, I use a standard hyphen for clarity (by adding @code{#\-} as an argument).}

@examples[#:eval my-eval
     (hyphenate "ergo polymorphic")
     (hyphenate "ergo polymorphic" #\-)
     (hyphenate "ergo polymorphic" #:min-length 13)
     (hyphenate "ergo polymorphic" #:min-length #f)
   ]

Because the hyphenation is based on an algorithm rather than a dictionary, it makes good guesses with unusual words:

@examples[#:eval my-eval
    (hyphenate "scraunched strengths" #\-)
     (hyphenate "RacketCon" #\-)
     (hyphenate "supercalifragilisticexpialidocious" #\-)
   ]

Using the @racket[#:exceptions] keyword, you can pass hyphenation exceptions as a list of words with hyphenation points marked with regular hyphens (@racket["-"]). If an exception word contains no hyphens, that word will never be hyphenated.

@examples[#:eval my-eval
     (hyphenate "polymorphic" #\-)
     (hyphenate "polymorphic" #\- #:exceptions '("polymo-rphic"))
     (hyphenate "polymorphic" #\- #:exceptions '("polymorphic"))
   ]

Knuth & Liang were sufficiently confident about their algorithm that they originally released it with only 14 exceptions: @italic{associate[s], declination, obligatory, philanthropic, present[s], project[s], reciprocity, recognizance, reformation, retribution}, and @italic{table}. Admirable bravado, but it's not hard to discover others that need adjustment.

@examples[#:eval my-eval
     (hyphenate "wrong: columns signage lawyers" #\-) 
     (hyphenate "right: columns signage lawyers" #\- 
     #:exceptions '("col-umns" "sign-age" "law-yers")) 
   ]

Overall, my impression is that the Knuth–Liang algorithm is more likely to miss legitimate hyphenation points (i.e., generate false negatives) than create erroneous hyphenation points (i.e., false positives). This is good policy. Perfect hyphenation — that is, hyphenation that represents an exact linguistic syllabification of each word — is superfluous for typesetting. Hyphenation simply seeks to mark possible line-break and page-break locations for whatever layout engine is drawing the text. The ultimate goal is to permit more even text flow. Like horseshoes and hand grenades, close is good enough. And a word wrongly hyphenated is more likely to be noticed by a reader than a word inefficiently hyphenated.

For this reason, certain words can't be hyphenated algorithmically, because the correct hyphenation depends on meaning, not merely on spelling. For instance:

@examples[#:eval my-eval
     (hyphenate "adder") 
   ]
   
This is the right result. If you used @italic{adder} to mean the machine, it would be hyphenated @italic{add-er}; if you meant the snake, it would be @italic{ad-der}. Better to avoid hyphenation than to hyphenate incorrectly.

You can send HTML-style X-expressions through @racket[hyphenate]. It will recursively hyphenate the text strings, while leaving the tags and attributes alone, as well as non-hyphenatable material (like character entities and CDATA).

@examples[#:eval my-eval
     (hyphenate '(p "strangely" (em "formatted" (strong "snowmen"))) #\-)
     (hyphenate '(headline [[class "headline"]] "headline") #\-)
      (hyphenate '(div "The (span epsilon) entity:" epsilon) #\-) 
   ]

Don't send raw HTML or XML through @racket[hyphenate]. It can't distinguish tags and attributes from textual content, so everything will be hyphenated, thus goofing up your file. But you can easily convert your HTML or XML to an X-expression, hyphenate it, and then convert back.

@examples[#:eval my-eval
    (define html "<body style=\"background: yellow\">Hello</body>")
    (hyphenate html #\-)
    (xexpr->string (hyphenate (string->xexpr html) #\-)) 
   ]   

If you're working with HTML, be careful not to include any @code{<script>} or @code{<style>} blocks, which contain non-hyphenatable data. You can protect that data by using the @racket[#:omit-txexpr] keyword to specify a @racket[_txexpr-test]. The test will be applied to all tagged X-expressions (see @racket[txexpr?]). When @racket[_txexpr-test] evaluates to true, the item will be skipped.

@examples[#:eval my-eval
     (hyphenate '(body "processing" (script "no processing")) #\-)
     (hyphenate '(body "processing" (script "no processing")) #\- 
     #:omit-txexpr (λ(tx) (member (get-tag tx) '(script))))
]

You can also use @racket[#:omit-txexpr] to omit tagged X-expressions with particular attributes. This can be used to selectively suppress hyphenation at the markup level.

@examples[#:eval my-eval
     (hyphenate '(p (span "processing") (span [[klh "no"]] "processing")) #\-)
     (hyphenate '(p (span "processing") (span [[klh "no"]] "processing")) #\-
     #:omit-txexpr (λ(tx) (and (attrs-have-key? tx 'klh) 
     (equal? (attr-ref tx 'klh) "no"))))
]

Similarly, you can use the @racket[#:omit-word] argument to avoid words that match @racket[_word-test]. Convenient if you want to prevent hyphenation of certain sets of words, like proper names:

@examples[#:eval my-eval
     (hyphenate "Brennan Huff likes fancy sauce" #\-) 
     (define capitalized? (λ(word) (let ([letter (substring word 0 1)])
 (equal? letter (string-upcase letter)))))
     (hyphenate "Brennan Huff likes fancy sauce" #:omit-word capitalized? #\-) 
   ]
   
Sometimes you need @racket[#:omit-word] to prevent unintended consequences. For instance, if you're using ligatures in CSS, certain groups of characters (fi, fl, ffi, et al.) will be replaced by a single glyph. That looks snazzy, but adding soft hyphens between any of these pairs will defeat the ligature substitution, creating inconsistent results. With @racket[#:omit-word], you can skip these words:

@margin-note{``Wouldn't it be better to exclude certain pairs of letters rather than whole words?'' Yes. But for now, that's not supported.}

@examples[#:eval my-eval
(hyphenate "Hufflepuff golfing final on Tuesday" #\-)
(define (ligs? word)
  (ormap (λ(lig) (regexp-match lig word)) 
  '("ff" "fi" "fl" "ffi" "ffl")))
(hyphenate "Hufflepuff golfing final on Tuesday" #:omit-word ligs? #\-) 
]


@defproc[
(unhyphenate
[xexpr xexpr/c] 
[joiner (or/c char? string?) @(integer->char #x00AD)]
[#:omit-word word-test (string? . -> . any/c) (λ(x) #f)]
[#:omit-string string-test (string? . -> . any/c) (λ(x) #f)]
[#:omit-txexpr txexpr-test (txexpr? . -> . any/c) (λ(x) #f)])
xexpr/c]
Remove @racket[_joiner] from @racket[_xexpr]. Like @racket[hyphenate], it works on nested X-expressions, and offers the same @racket[#:omit-] options.

@examples[#:eval my-eval
     (hyphenate '(p "strangely" (em "formatted" (strong "snowmen"))) #\-)
     (unhyphenate '(p "strange-ly" (em "for-mat-ted" (strong "snow-men"))) #\-)
]

A side effect of using @racket[hyphenate] is that soft hyphens (or whatever the @racket[_joiner] is) will be embedded in the output text. If you need to support copying of text, for instance in a GUI application, you'll probably want to strip out the hyphenation before the copied text is moved to the clipboard.

@examples[#:eval my-eval
     (hyphenate "ribbon-cutting ceremony") 
     (unhyphenate (hyphenate "ribbon-cutting ceremony"))
     ]

Use this function cautiously — if @racket[_joiner] appeared in the original input to @racket[hyphenate], the output from @racket[unhyphenate] won't be the same string.

@examples[#:eval my-eval
     (hyphenate "ribbon-cutting ceremony" #\-) 
     (unhyphenate (hyphenate "ribbon-cutting ceremony" #\-) #\-)
    ]
    
Keep in mind that soft hyphens could appear in your input string. Certain word processors allow users to @link["http://practicaltypography.com/optional-hyphens.html"]{insert soft hyphens} in their text.

@examples[#:eval my-eval
     (hyphenate "True\u00ADType typefaces")
     (unhyphenate (hyphenate "True\u00ADType typefaces"))
     (hyphenate (unhyphenate "True\u00ADType typefaces") #\-)
   ]


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/hyphenate"]{http://github.com/mbutterick/hyphenate}. Suggestions & corrections welcome.

