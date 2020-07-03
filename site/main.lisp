(uiop:define-package :ballish/site/main
    (:use :cl)
  (:import-from #:hunchentoot :start :easy-acceptor)
  (:import-from #:uiop :getenv)
  (:export #:main))

(in-package :ballish/site/main)

(defun start-server (&optional port)
  (start (make-instance 'easy-acceptor
			:port (or port (parse-integer (getenv "PORT")))
			:document-root (merge-pathnames
					#p"site/static/"
					(asdf:system-source-directory :ballish/site/main)))))
