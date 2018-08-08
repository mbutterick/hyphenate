hyphenate [![Build Status](https://travis-ci.org/mbutterick/hyphenate.svg?branch=master)](https://travis-ci.org/mbutterick/hyphenate)
---------

Racket implementation of the Knuth–Liang hyphenation algorithm.

Install from the command line like so:

    raco pkg install hyphenate
    
Later, you an update like so:

    raco pkg update hyphenate

Import in standard mode:

    (require hyphenate)
    
In safe mode (with contracts):

    (require (submod hyphenate safe))
    
And enjoy:

    (hyphenate "Hyphenation algorithm" #\-)
    
    > "Hy-phen-ation al-go-rithm"
    
Full docs are installed with the package. You can also [read the docs here](http://pkg-build.racket-lang.org/doc/hyphenate).
