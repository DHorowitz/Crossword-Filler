===================================================================================
ARTIFICIAL INTELLIGENCE FINAL PROJECT
DYLAN HOROWITZ '19
CROSSWORD SOLVER/FILLER
===================================================================================

This folder contains the following files:
     basics.lisp
     variables.lisp
     crossword-starter.lisp
     csp.lisp
     words.txt

In order to properly run this program, first run the following:

   (compile-file "basics.lisp")
   (load "basics")
   (maker)
   (maker)

"maker" is a function within basics.lisp that compiles the rest of the files. However,
this must run twice in order to work, due to multiple variables and functions being
used cross-file (csp-node from csp.lisp in crossword-starter.lisp and crossword vise-versa)

After this, creating a new crossword game requires the grid and variables. For example,
to create a crossword named game using the first grid test (A1):

   (setf game (new-crossword *A1* *A1Variables*))

After this, simply calling the function "fill-crossword" with the crossword struct will fill
the crossword as desired. Taking our above example:

    (fill-crossword game)

If you wish to see the initial board before anything is put in it, simply use the "print-crossword"
function before fill-crossword. All this takes is the crossword struct and where to send it out to.
Again with our example:
      (print-crossword game t)

After running fill-crossword, if you desire to try a different crossword design, you will have to run
"maker" again (only once this time) before setting your variable to the new crossword.

There is a slight element of chance to the crosswords, so each implementation of fill-crossword will
have slightly different results.

Happy crosswording!

PLEASE NOTE: This crossword filler currently only works in two of the given sample cases. Any case that involves backtracking currently ends in an infinite loop, which will hopefully be fixed in later iterations of this program.
