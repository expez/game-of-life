# Conway's game of life

This is a basic implementation of Conway's game of life in common lisp.  I wrote this while learning common lisp because I wanted a tiny project I could use to check out the ncurses bindings available in cl.  Thus, this little program uses cl-charms to output to the terminal.

There are a ton of interesting structures available for game of life.  While looking around I found [this glossary](http://www.radicaleye.com/lifepage/glossary.html).  I removed anything which isn't actually a pattern and saved the rest in the file `patterns`.  This program treats the glossary as data and we can instantiate any patterns in that list.

## Usage

`sbcl --load game-of-life.lisp <name-of-pattern> <simulation duration in seconds>`

Any spaces in the name of the pattern has to be replaced with a `-`.  The default duration for the simulation is 20s.

`sbcl --load game-of-life.lisp cis-boat-with-tail 30` will display the cis-boat with tail pattern for 30s.

By replacing the pattern name with the secret keyword `all` a loop is started which loops through all the available patterns.

### Dependencies

* SBCL
* [Quicklisp](www.quicklisp.org)
