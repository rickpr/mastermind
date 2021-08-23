Solves [mastermind](https://www.pressmantoy.com/product/mastermind-2/) in 5 moves or less using
[Knuth's mastermind algorithm](https://www.cs.uni.edu/~wallingf/teaching/cs3530/resources/knuth-mastermind.pdf).
# Installation

## Vanilla Emacs
First, clone the repository:

```
git clone https://github.com/rickpr/mastermind.git
```

Then, from within Emacs:

- `M-x package-install-file`
- Select `mastermind.el` from the folder the repo was cloned into (should be `mastermind`).

## DOOM Emacs
Add to your `packages.el`:
``` elisp
(package! mastermind
  :recipe (:host github :repo "rickpr/mastermind"))
```

Then run `doom sync -u` (usually invoked by `~/.emacs.d/bin/doom sync -u`) to install.

# Usage
Choose a mastermind code and press `M-x mastermind`. The computer will make a guess.
Let the computer know how many red hits and white hits it has, and it will refine its
guess until it finds the combination. Should take no more than 5 guesses.

# Running tests
Open the file `mastermind-test.el` and run `M-x eval-buffer`. Then run `M-x ert`.
