#lang scribble/manual

@(require scribble/eval "../main.rkt" (for-label racket "../main.rkt"))

@defmodule[hyphenate]

@title{Hyphenate}

@author{Matthew Butterick (mb@"@"mbtype.com)}

A simple hyphenation module that uses the Knuth–Liang hyphenation algorithm and patterns originally developed for TeX. This implementation was ported from Ned Batchelder's @link["http://nedbatchelder.com/code/modules/hyphenate.html"]{Python version}. 

I originally developed this module to handle hyphenation for my web-based book @link["http://practicaltypography.com"]{Butterick's Practical Typography}. Even though support for CSS-based hyphenation is still iffy among web browsers, soft hyphens work reliably.

@section{How to use it}

@section{Interface}

@defproc[
(hyphenate 
[text string?] 
[joiner (or/c char? string?) @(integer->char #x00AD)]
[#:exceptions exceptions (listof string?) empty]
[#:min-length length (or/c integer? false?) 5])
string?]
Hyphenate @racket[_text] by calculating hyphenation points and inserting @racket[_joiner] at those points. By default, @racket[_joiner] is the soft hyphen. Words shorter than @racket[_length] will not be hyphenated. To hyphenate words of any length, use @racket[#:min-length] @racket[#f].

@margin-note{The REPL will display a soft hyphen as #\u00AD. But in ordinary use, you only see a soft hyphen when it appears at the end of a line or page as part of a hyphenated word. Otherwise it's invisible.}

Using the @racket[#:exceptions] keyword, you can pass hyphenation exceptions  as a list of words with regular hyphen characters (@racket["-"]) marking the permissible hyphenation points. If an exception word contains no hyphens, that word will never be hyphenated.

@examples[
     (hyphenate "polymorphism" #\-)
     (hyphenate "polymorphism" #\- #:exceptions '("polymo-rphism"))
     (hyphenate "polymorphism" #\- #:exceptions '("polymorphism"))
   ]

Knuth & Liang were sufficiently confident about their algorithm that they originally released it with only 14 exceptions: @italic{associate[s], declination, obligatory, philanthropic, present[s], project[s], reciprocity, recognizance, reformation, retribution}, and @italic{table}. While their bravado is admirable, it's easy to discover words they missed.

Don't send raw HTML through @racket[hyphenate]. It can't distinguish HTML tags and attributes from textual content, but it will hyphenate them anyhow, which will break the markup. Run your textual content through @racket[hyphenate] before you put it into your page template. Or convert your HTML to an X-expression and process it selectively.

@defproc[
(hyphenatef
[text string?] 
[pred procedure?]
[joiner (or/c char? string?) @(integer->char #x00AD)]
[#:exceptions exceptions (listof string?) empty]
[#:min-length length (or/c integer? false?) 5])
string?]
Like @racket[hyphenate], but only words matching @racket[_pred] are hyphenated. Convenient if you want to filter out, say, capitalized words.

@defproc[
(unhyphenate
[text string?] 
[joiner (or/c char? string?) @(integer->char #x00AD)])
string?]
Remove @racket[_joiner] from @racket[_text]. Essentially equivalent to (@racket[string-replace] @racket[_text] @racket[_joiner] "").

A side effect of using @racket[hyphenate] is that soft hyphens (or whatever the @racket[_joiner] is) are being embedded in the output @racket[_text]. If you're building an application that needs to support, for instance, copying of text in a graphical interface, you probably want to strip out the hyphenation before the copied text is moved to the clipboard.

Keep in mind, however, that @racket[unhyphenate] won't produce the input originally passed to @racket[hyphenate] if the @racket[_joiner] was part of the original input @racket[_text].

