;;; mastermind.el --- solves mastermind
;; Version: 1.0
;;; Commentary:
;;
;; Description:
;;
;;; Code:
(defun repeated-permutation (list size) "Return the permutations of LIST with SIZE." (
    if (or (null list) (= 0 size)) '(()) (

    apply #'append (
      mapcar (
        lambda (x) (
          mapcar (lambda (permutation) (cons x permutation)) (repeated-permutation list (- size 1))
        )
      )
      list
    )
  )
))

(defun remove-first (element-to-remove original-list)
  "Remove the first occurence of ELEMENT-TO-REMOVE from ORIGINAL-LIST. Note this doesn't preserve order."
  (let ((list-with-element-removed) (element-has-been-found nil))
    (dolist (element original-list list-with-element-removed)
      (if (and (not element-has-been-found) (equal element element-to-remove))
        (setq element-has-been-found t)
        (setq list-with-element-removed (cons element list-with-element-removed))
      )
    )
  )
)

(defun min-by (mapping-function list)
  "Return the minimum element of LIST based on MAPPING-FUNCTION."
  (if (not list) nil

    (let* ((min-element (car list)) (min-value (funcall mapping-function (car list))) (element))
      (dolist (element (cdr list) min-element)
        (let ((mapped-element (funcall mapping-function element)))
          (if (string< mapped-element min-value)
            (progn (setq min-element element) (setq min-value mapped-element))
            nil
          )
        )
      )
      min-element
    )
  )
)

(defun zip-lists (first-list second-list)
  "Create pairs between each successive element of FIRST-LIST and SECOND-LIST.
If FIRST-LIST is longer, pairs each extra element with NIL.
If SECOND-LIST is longer, this stops at the end of FIRST-LIST."
  (let ((index -1))
    (mapcar
      (lambda (element) (progn
        (setq index (+ 1 index))
        (list element (elt second-list index))
      ))
      first-list
    )
  )
)

(defun elements-in-common (first-list second-list)
  "Get elements in common between FIRST-LIST and SECOND-LIST, counting duplicates."
  (let (common-elements)
    (dolist (element first-list common-elements)
      (if (member element second-list)
        (progn
          (setq second-list (remove-first element second-list))
          (setq common-elements (cons element common-elements))
        )
        nil
      )
    )
  )
)

(defun check-guess (code guess)
  "Return pegs for an individual GUESS against a CODE."
  (let* (
      (pairs (zip-lists code guess))
      (non-red-hits (seq-remove (lambda (color) (apply #'string= color)) pairs))
      (red-hits-count (- (length code) (length non-red-hits)))
      (unchecked-code-elements (mapcar (lambda(pair) (car pair)) non-red-hits))
      (unchecked-guess-elements (mapcar (lambda(single) (car (last single))) non-red-hits))
      (white-hits-count (length (elements-in-common unchecked-code-elements unchecked-guess-elements)))
    )
    (list red-hits-count white-hits-count)
  )
)

(defun code-breaker (codemaker-check-guess) "Breaks a mastermind code with feedback from CODEMAKER-CHECK-GUESS."
  (let* (
      (colors '("blue" "red" "white" "green" "black" "yellow"))
      (initial-guess '("blue" "blue" "red" "red"))
      (guess initial-guess)
      (guesses (make-hash-table :test 'equal))
      (all-possibilities (repeated-permutation colors 4))
      (possibilities (repeated-permutation colors 4))
    )

    (defun break-code! ()
      (make-guess)
      (if (= 4 (car (gethash guess guesses)))
        guess
        (progn
          (setq possibilities (calculate-remaining-possibilities guess (gethash guess guesses)))
          (setq guess (find-best-guess))
          (if possibilities nil '("Invalid input, no possibilities remain."))
        )
      )
    )

    (defun make-guess ()
      "Makes a guess. Expects feedback from the codemaker. Put thinking message here as we will think after."
      (puthash guess (funcall codemaker-check-guess guess) guesses)
      (display-message-or-buffer "Thinking, please wait...")
    )

    (defun calculate-remaining-possibilities (guess result)
      "Enumerate possibilities that remain after the RESULT of the current GUESS."
      (seq-filter (lambda (possibility) (equal result (check-guess guess possibility))) (remove guess possibilities))
    )

    (defun find-best-guess ()
      (if (<= (length possibilities) 2) (car possibilities) (progn

        (let* (
            (unguessed-possibilities (seq-remove (lambda (possibility) (gethash possibility guesses)) all-possibilities))
            (all-guesses
              (mapcar (
                lambda (possible-guess) (progn
                  (let ((results-with-counts (make-hash-table :test 'equal)))
                    (dolist (possibility possibilities)
                      (let ((result (check-guess possible-guess possibility)))
                        (puthash result (+ 1 (gethash result results-with-counts 0)) results-with-counts)))
                    (let ((highest-count (apply #'max (hash-table-values results-with-counts))))
                      (list possible-guess highest-count))
                )))
                unguessed-possibilities)
              )
            (worst-case-smallest-group (
              apply #'min (mapcar (lambda (guess-and-count) (car (last guess-and-count))) all-guesses)
            ))
            (best-guesses (
              mapcar #'car (
                seq-filter (lambda(guess-with-count) (= worst-case-smallest-group (car (last guess-with-count)))) all-guesses
              )
            ))
          )
          (min-by (
              lambda (good-guess) (concat (if (member good-guess possibilities) "a" "b") (string-join good-guess)))
            best-guesses
          )
        )
      ))
    )
    (let (
        (solution nil)
        (number-of-guesses 0)
      )
      (while (eq nil solution)
        (setq solution (break-code!))
        (setq number-of-guesses (+ 1 number-of-guesses))
      )
      (display-message-or-buffer
        (concat
          "Guesses: " (number-to-string number-of-guesses)
          ", solution: " (string-join solution ", "))
      )
      solution
    )
  )
)

(defun human-code-maker (guess)
  "Prints the GUESS out and has the human enter the hit counts."
  (let (
      (red-hits (read-number (concat (string-join guess ", ") " Red hits: ")))
      (white-hits (read-number (concat (string-join guess ", ") " White hits: ")))
    )
    (list red-hits white-hits)
  )
)

;;;###autoload
(defun mastermind ()
  "Will guess a mastermind code by using feedback a human gives."
  (interactive)
  (code-breaker 'human-code-maker))

(provide 'mastermind)
;;; mastermind.el ends here
