;;; Mastermind --- solves mastermind
;;; Commentary: First makes a guess then takes back peg colors and counts as input.

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

(defun check-guess (code guess)
  "Return pegs for an individual GUESS against a CODE."
  (setq pairs (-zip-lists code guess))
  (setq non-red-hits (seq-remove (lambda (color) (apply #'string= color)) pairs))
  (setq red-hits-count (- (length code) (length non-red-hits)))
  (setq unchecked-code-elements (mapcar (lambda(pair) (car pair)) non-red-hits))
  (setq unchecked-guess-elements (mapcar (lambda(single) (car (last single))) non-red-hits))
  (setq white-hits-count (
    length (
      seq-filter (
        lambda (color) (
          if (member color unchecked-guess-elements) (progn
              (setq unchecked-code-elements (--remove-first (string= it color) unchecked-code-elements))
              t
            )
            nil
        ))
        unchecked-code-elements)))
  (list red-hits-count white-hits-count)
)

(defun code-breaker (codemaker-check-guess) "Breaks a mastermind code with feedback from CODEMAKER-CHECK-GUESS."
  (setq colors '("blue" "red" "white" "green" "black" "yellow"))
  (defun enumerate-possibilities () "All possible guess." (repeated-permutation colors 4))
  (setq initial-guess '("blue" "blue" "red" "red"))
  (setq guess initial-guess)
  (setq guesses (make-hash-table :test 'equal))
  (setq all-possibilities (enumerate-possibilities))
  (setq possibilities (enumerate-possibilities))

  (defun break-code! ()
    (with-output-to-string (princ "Guessing ") (princ guess))
    (make-guess)
    (if (= 4 (car (gethash guess guesses))) guess (progn
      (setq possibilities (calculate-remaining-possibilities guess (gethash guess guesses)))
      (print (length possibilities))
      (setq guess (find-best-guess)))
    nil)
  )

  (defun make-guess () (puthash guess (funcall codemaker-check-guess guess) guesses))

  (defun calculate-remaining-possibilities (guess result)
    "Enumerate possibilities that remain after the RESULT of the current GUESS."
    (seq-filter (lambda (possibility) (equal result (check-guess guess possibility))) (remove guess possibilities))
  )

  (defun find-best-guess ()
    (if (<= (length possibilities) 2) (car possibilities) (progn

      (setq unguessed-possibilities (seq-remove (lambda (possibility) (gethash possibility guesses)) all-possibilities))
      (setq all-guesses
        (mapcar (
          lambda (possible-guess) (progn
            (setq results-with-counts (make-hash-table :test 'equal))
            (mapc (
                lambda (possibility) (progn
                  (setq result (check-guess possible-guess possibility))
                  (puthash result (+ 1 (gethash result results-with-counts 0)) results-with-counts)
                ))
            possibilities)
            (setq highest-count (
              apply #'max (hash-table-values results-with-counts)
            ))
            (list possible-guess highest-count)
          ))
          unguessed-possibilities)
      )
      (setq worst-case-smallest-group (
        apply #'min (mapcar (lambda (guess-and-count) (car (last guess-and-count))) all-guesses)
      ))
      (setq best-guesses (
        mapcar #'car (
          seq-filter (lambda(guess-with-count) (= worst-case-smallest-group (car (last guess-with-count)))) all-guesses
        )
      ))
      (--min-by (
          lambda (good-guess) (concat (if (member good-guess possibilities) "a" "b") (string-join good-guess)))
        best-guesses
      )
    ))
  )
  (setq solution nil)
  (setq number-of-guesses 0)
  (while (eq nil solution)
    (setq solution (break-code!))
    (setq number-of-guesses (+ 1 number-of-guesses))
  )
  (with-output-to-string (princ "Guesses ") (princ number-of-guesses))
)

(defun human-code-maker (guess)
  "Prints the GUESS out and has the human enter the hit counts."
  (setq red-hits (read-number (concat (string-join guess ", ") " Red hits: ")))
  (setq white-hits (read-number (concat (string-join guess ", ") " White hits: ")))
  (list red-hits white-hits)
)

;;;###autoload
(defun mastermind ()
  (interactive)
  (code-breaker 'human-code-maker))

(provide 'mastermind)
;;; mastermind.el ends here
