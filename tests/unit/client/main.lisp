(uiop:define-package :ballish/tests/unit/client/main
    (:use :cl :fiveam))

(in-package :ballish/tests/unit/client/main)

(defvar *cwd* (uiop:getcwd))
(defvar *cwd-str* (namestring *cwd*))

(test normalize-folder
  (is (string= *cwd-str*
	       (ballish/client/main::normalize-folder ".")))

  (is (string= *cwd-str*
	       (ballish/client/main::normalize-folder
		(subseq *cwd-str* 0 (1- (length *cwd-str*))))))

  (is (string= *cwd-str*
	       (ballish/client/main::normalize-folder
		(concatenate 'string
			     *cwd-str* "../" (first (last (pathname-directory *cwd*))))))))
