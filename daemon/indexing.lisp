(uiop:define-package :ballish/daemon/indexing
    (:use :cl :ballish/daemon/elasticsearch)
  (:import-from :sb-thread #:make-thread #:terminate-thread)
  (:import-from :sb-concurrency #:receive-message)
  (:import-from :sb-posix #:stat #:stat-mtime)
  (:export #:with-indexing-thread))

(in-package :ballish/daemon/indexing)

(defmacro with-indexing-thread ((queue) &body body)
  (let ((thread (gensym)))
    `(let ((,thread (make-thread #'index :arguments (list ,queue))))
       (unwind-protect
	    (progn ,@body)
	 (terminate-thread ,thread)))))

(defvar *text-extensions* '("py" "cfg"))

(defun index (queue)
  (loop
     (let ((path (receive-message queue)))
       (when (member (pathname-type path) *text-extensions* :test #'string=)
	 (index-code path (stat-mtime (stat path)) (uiop:read-file-string path))))))
