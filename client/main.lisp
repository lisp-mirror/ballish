(uiop:define-package :ballish/client/main
    (:use :cl)
  (:export #:main))

(in-package :ballish/client/main)

(defun main ()
  (format t "Hello, world!~%"))
