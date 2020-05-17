(uiop:define-package :ballish/tests/unit/client/main
    (:use :cl :fiveam))

(in-package :ballish/tests/unit/client/main)

(uiop:chdir #p"/tmp/")

(test normalize-folder
  (is (string= "/tmp/"
	       (ballish/client/main::normalize-folder ".")))

  (is (string= "/tmp/"
	       (ballish/client/main::normalize-folder "/tmp")))

  (is (string= "/tmp/"
	       (ballish/client/main::normalize-folder "/tmp/../tmp"))))
