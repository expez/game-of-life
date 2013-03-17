# Conway's game of life

This is a super basic implementation of Conway's game of life in common lisp.  I wrote this while learning common lisp because I wanted a tiny project I could use to check out the ncurses bindings available in cl.  Thus, this little program uses cl-charms to output to the terminal.

If you're looking for a game of life implementation to play with, this is unfortunately not the place to look.  I haven't taken the time to implement an interface to put interesting structures like the `glider` or `spaceship` onto the board.  Instead the user is left to edit the source code itself.
