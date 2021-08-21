;;; mastermind-test.el --- Tests for mastermind solver
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(load-file "./mastermind.el")

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

;;; mastermind-test.el ends here
