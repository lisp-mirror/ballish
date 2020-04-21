(uiop:define-package :ballish/daemon/indexing
    (:use :cl :iterate :ballish/daemon/elasticsearch)
  (:import-from :sb-thread #:make-thread #:terminate-thread)
  (:import-from :sb-concurrency #:receive-message)
  (:import-from :sb-posix #:stat #:stat-mtime #:syscall-error #:syscall-errno)
  (:import-from :sb-int #:stream-decoding-error)
  (:import-from :alexandria #:alist-hash-table #:hash-table-keys)
  (:export #:with-indexing-thread))

(in-package :ballish/daemon/indexing)

(defmacro with-indexing-thread ((queue) &body body)
  (let ((thread (gensym)))
    `(let ((,thread (make-thread #'index :arguments (list ,queue) :name "Indexing thread")))
       (unwind-protect
	    (progn ,@body)
	 (terminate-thread ,thread)))))

(defvar *text-extensions-tags* (alist-hash-table '(("py" . ("python"))
						   ("cfg" . ("python"))
						   ("go" . ("golang")))
						 :test #'equal))

(defvar *text-extensions* (hash-table-keys *text-extensions-tags*))

(defun cdr-assoc (item alist)
  (cdr (assoc item alist)))

(defun index (queue)
  (loop
     (let* ((path (receive-message queue))
	    (path-type (pathname-type path)))
       (when (member path-type *text-extensions* :test #'string=)
	 (handler-case
	     (let ((s (stat path)))
	       (index-code path
			   (stat-mtime s)
			   (uiop:read-file-string path)
			   (gethash path-type *text-extensions-tags*)))
	   (syscall-error (e)
	     (when (= (syscall-errno e) 2)
	       (deindex-code path)))
	   (stream-decoding-error ()
	     ;; Ignore non-valid-utf-8 files
	     nil))))))
