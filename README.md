## hyphenate ![Build Status](https://github.com/mbutterick/hyphenate/workflows/CI/badge.svg)


Racket implementation of the Knuth–Liang hyphenation algorithm.

Install from the command line like so:

    raco pkg install hyphenate
    
Later, you an update like so:

    raco pkg update hyphenate

Import in standard mode:

    (require hyphenate)
    
Or in safe mode (adds contracts):

    (require (submod hyphenate safe))
    
And enjoy:

    (hyphenate "Hyphenation algorithm" #\-)
    
    > "Hy-phen-ation al-go-rithm"
    
Full docs are installed with the package. You can also [read the docs here](http://pkg-build.racket-lang.org/doc/hyphenate).


## Project status

Complete. Maintained but no further updates planned. Once upon a time, I had an ambition to support more languages. But I don’t personally need that, and no one is clamoring for it.
