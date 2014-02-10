#lang scribble/manual

@(require (for-label racket "../main.rkt"))

@defmodule[hyphenate]

@title{Hyphenate}

@author{Matthew Butterick (mb@"@"mbtype.com)}

An implementation of the Knuth–Liang hyphenation algorithm originally developed for TeX. This version was ported from Ned Batchelder's @link["http://nedbatchelder.com/code/modules/hyphenate.html"]{Python version}. 

@margin-note{I developed this library to handle hyphenation for my web-based book @link["http://practicaltypography.com"]{Butterick's Practical Typography}. Even though support for CSS-based hyphenation is still iffy among web browsers, soft hyphens work reliably.}

@section{Quick start}

@section{Interface}

@defproc[
(hyphenate 
[text string?] [joiner char? @(integer->char #x00AD)]
[#:exceptions exceptions (listof string?) empty])
string?]
Returns a hyphenated copy of @racket[_text] with @racket[_joiner] inserted at the hyphenation points. By default, @racket[_joiner] is the soft hyphen.

@margin-note{Unlike a regular hyphen, you only see a soft hyphen when it appears at the end of a line or page as part of a hyphenated word. Otherwise it's invisible.}

Using the @racket[#:exceptions] keyword, you can pass hyphenation exceptions  as a list of words with regular hyphen characters (@racket["-"]) marking the permissible hyphenation points. If an exception word contains no hyphens, that word will never be hyphenated.

Famously, Knuth & Liang were so confident about their algorithm that they originally released it with only 14 exceptions: @italic{associate[s], declination, obligatory, philanthropic, present[s], project[s], reciprocity, recognizance, reformation, retribution}, and @italic{table}. While their bravado is admirable, it's easy to discover words they missed.

@defproc[
(hyphenatef
[text string?] 
[pred procedure?]
[joiner char? @(integer->char #x00AD)]
[#:exceptions exceptions (listof string?) empty])
string?]
Like @racket[hyphenate], but only words matching @racket[_pred] are hyphenated. Convenient if you want to avoid, for instance, capitalized words.

@section{Caveats}

@section{License}
