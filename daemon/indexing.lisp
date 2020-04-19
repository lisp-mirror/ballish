(uiop:define-package :ballish/daemon/indexing
    (:use :cl)
  (:import-from :sb-thread #:make-thread #:terminate-thread)
  (:import-from :sb-concurrency #:receive-message)
  (:export #:with-indexing-thread))

(in-package :ballish/daemon/indexing)

(defmacro with-indexing-thread ((queue uri) &body body)
  (let ((thread (gensym)))
    `(let ((,thread (make-thread #'index :arguments (list ,queue ,uri))))
       (unwind-protect
	    (progn ,@body)
	 (terminate-thread ,thread)))))

(defvar *text-extensions* '("py" "cfg"))

(defun index (queue es-uri)
  (loop
     (let ((path (receive-message queue)))
       (when (member (pathname-type path) *text-extensions* :test #'string=)
	 (format t "About to index ~a~%" path)))))
