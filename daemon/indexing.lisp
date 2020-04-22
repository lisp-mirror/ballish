(uiop:define-package :ballish/daemon/indexing
    (:use :cl :iterate)
  (:import-from :sb-thread #:make-thread #:terminate-thread)
  (:import-from :sb-concurrency #:receive-message)
  (:import-from :sb-posix #:stat #:stat-mtime #:syscall-error #:syscall-errno)
  (:import-from :sb-int #:stream-decoding-error)
  (:import-from :alexandria #:alist-hash-table #:hash-table-keys)
  (:import-from :montezuma
		#:index #:document
		#:get-document #:document-value
		#:delete-document
		#:add-field #:make-field #:add-document-to-index)
  (:export #:with-indexing-thread))

(in-package :ballish/daemon/indexing)

(defvar *text-extensions-tags* (alist-hash-table '(("py" . ("python"))
						   ("cfg" . ("python"))
						   ("go" . ("golang")))
						 :test #'equal))

(defvar *text-extensions* (hash-table-keys *text-extensions-tags*))

(defvar *source-index* nil
  "The index used to store source code.")

(defmacro with-indexing-thread ((queue) &body body)
  (let ((thread (gensym)))
    `(progn
       (setf *source-index* (make-instance 'index :path (index-path #p"source")))
       (let ((,thread (make-thread #'index-loop :arguments (list ,queue) :name "Indexing thread")))
	 (unwind-protect
	      (progn ,@body)
	   (terminate-thread ,thread))))))

(defun index-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun cdr-assoc (item alist)
  (cdr (assoc item alist)))

(defun index-loop (queue)
  (loop
     (let* ((path (receive-message queue))
	    (path-type (pathname-type path)))
       (when (member path-type *text-extensions* :test #'string=)
	 (handler-case
	     (let ((s (stat path)))
	       (index-source path
			     (stat-mtime s)
			     (uiop:read-file-string path)
			     (gethash path-type *text-extensions-tags*)))
	   (syscall-error (e)
	     (when (= (syscall-errno e) 2)
	       (deindex-source path)))
	   (stream-decoding-error ()
	     ;; Ignore non-valid-utf-8 files
	     nil))))))

(defun index-source (path mtime content tags)
  (let* ((path-str (namestring path))
	 (existing-doc (get-document *source-index* path-str)))
    (when (or (not existing-doc)
	      (> mtime (parse-integer (document-value existing-doc "mtime"))))
      (let ((doc (make-instance 'document)))
	(add-field doc (make-field "id" (namestring path) :index :untokenized))
	(add-field doc (make-field "mtime" (write-to-string mtime) :index nil))
	(add-field doc (make-field "content" content))
	(add-field doc (make-field "tags" (format nil "~{~a~^ ~}" tags)))

	(add-document-to-index *source-index* doc)))))

(defun deindex-source (path)
  (delete-document *source-index* (namestring path)))
