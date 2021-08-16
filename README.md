# Installation
This is the script foor DOOM emacs - should adapt to your package manager:
``` elisp
(package! mastermind
  :recipe (:host github :repo "rickpr/mastermind"
           :files ("mastermind.el" "src/lisp/*.el")))
```

Then you can press `M-x mastermind`. The computer will solve an ongoing
mastermind game if you tell it how many of each color pegs are present at each
turn.
