# ltk-small-games
Simplistic games written in Common Lisp with LTK for practice

Quicklisp is a prerequisite. For SBCL, do `sbcl --load [file]` to run the game.

I've found that if you want to run the file like (or from) a script, you can add the contents of your .sbclrc file (or the equivalent for whatever you use) to the beginning of the script. This works for running the game with `sbcl --script [file]` in case you want to run it from a bash/batch script for some reason, or if you want to add `#!/usr/bin/sbcl --script` to the top of your file and do `chmod +x [file]` to execute it. That way I don't have to learn how to actually use packages like a professional.

To-do: Add comments for everything
