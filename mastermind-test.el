;;; mastermind-test.el --- Tests for mastermind solver
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(load-file "./mastermind.el")

;; Testing check-guess function
(ert-deftest check-guess-when-colors-are-red-red-blue-blue-and-guess-is-blue-blue-red-red ()
  "Colors: red, red, blue blue. Guess: blue, blue, red, red. Should be 4 white hits."
  (should (equal
    (check-guess '("red" "red" "blue" "blue") '("blue" "blue" "red" "red"))
    (list 0 4)
  ))
)

(ert-deftest check-guess-when-colors-are-blue-blue-red-red-and-guess-is-red-black-blue-green ()
  "Colors: blue, blue, red red. Guess: red, black, blue, green. Should be 2 white hits."
  (should (equal
    (check-guess '("blue" "blue" "red" "red") '("red" "black" "blue" "green"))
    (list 0 2)
  ))
)

;; Testing remove-first function
(ert-deftest remove-first-only-no-occurrences ()
  "Test when the method removes nothing."
  (should (equal
    (remove-first 0 (list 1 2 3))
    (list 3 2 1)
  ))
)

(ert-deftest remove-first-only-one-occurrence ()
  "Test when the method removes one item."
  (should (equal
    (remove-first 1 (list 1 2 3))
    (list 3 2)
  ))
)

(ert-deftest remove-first-only-two-occurrences ()
  "Test when the method removes one item but it appears again."
  (should (equal
    (remove-first 1 (list 1 2 3 1))
    (list 1 3 2)
  ))
)

;; Testing elements-in-common function
(ert-deftest elements-in-common-no-duplicates ()
  "Check that the common elements of (1, 2, 3) and (2, 3, 4) are (3, 2)"
  (should (equal
    (elements-in-common (list 1 2 3) (list 2 3 4))
    (list 3 2)
  ))
)

(ert-deftest elements-in-common-duplicates ()
  "Check that the common elements of (1, 1, 2, 3) and (1, 1, 2, 2) are (2, 1, 1)"
  (should (equal
    (elements-in-common (list 1 1 2 3) (list 1 1 2 2))
    (list 2 1 1)
  ))
)

;; Testing min-by function
(ert-deftest min-by-empty-list ()
  "Should be NIL."
  (should (equal
    (min-by (lambda (x) (+ 1 x)) (list))
    nil
  ))
)

(ert-deftest min-by-one-element ()
  "Should be the element."
  (should (equal
    (min-by (lambda (x) (+ 1 x)) (list 1))
    1
  ))
)

(ert-deftest min-by-multiple-elements ()
  "Should be the largest element as we are subtracting it from 1."
  (should (equal
    (min-by (lambda (x) (- 1 x)) (list 1 2 3))
    3
  ))
)

;; Testing zip-lists function
(ert-deftest zip-lists-equal-length-lists ()
  (should (equal (zip-lists '(1 2) '(3 4)) '((1 3) (2 4))))
)

(ert-deftest zip-lists-first-list-longer ()
  (should (equal (zip-lists '(1 2) '(3)) '((1 3) (2 nil))))
)

(ert-deftest zip-lists-second-list-longer ()
  (should (equal (zip-lists '(1) '(3 4)) '((1 3))))
)

;; Testing code-breaker function (integration tests)
(ert-deftest code-breaker-red-red-blue-blue ()
  (let ((guess-count 0))
    (defun code-maker (guess)
      "Return the red and blue hits for blue blue, red, red and increment guess-count when called."
      (setq guess-count (+ 1 guess-count))
      (check-guess '("red" "red" "blue" "blue") guess)
    )
    (let ((solution (code-breaker 'code-maker)))
      (should (equal 2 guess-count))
      (should (equal '("red" "red" "blue" "blue") solution))
    )
  )
)

(ert-deftest code-breaker-all ()
  "Test every possibility and see it works on all. WARNING: takes a long time."
  (let ((possibilities (repeated-permutation '("blue" "red" "white" "green" "black" "yellow") 4)))
    (let ((solution-guess-counts
      (mapcar (lambda (possibility)
        (let ((guess-count 0))
          (defun code-maker (guess)
            "A code-maker with POSSIBILITY as the answer."
            (setq guess-count (+ 1 guess-count))
            (check-guess possibility guess)
          )
          (let ((solution (code-breaker 'code-maker)))
            (should (equal possibility solution))
            guess-count
          )
        ))
      possibilities)))
      (should (equal 5 (apply #'max solution-guess-counts)))
    )
  )
)
;;; mastermind-test.el ends here
