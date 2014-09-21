hyphenate
---------

Racket implementation of the Knuth–Liang hyphenation algorithm.

Install from the command line like so:

    raco pkg install hyphenate

Then require it in your Racket file:

    (require hyphenate)
    
And enjoy:

    (hyphenate "Hyphenation algorithm" #\-)
    
    > "Hy-phen-ation al-go-rithm"
    
Full docs are installed with the package. You can also [read the docs here](http://pkg-build.racket-lang.org/doc/hyphenate).
