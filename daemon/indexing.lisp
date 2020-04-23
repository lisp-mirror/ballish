(uiop:define-package :ballish/daemon/indexing
    (:use :cl :iterate)
  (:import-from :sb-thread #:make-thread #:terminate-thread)
  (:import-from :sb-concurrency #:receive-message)
  (:import-from :sb-posix #:stat #:stat-mtime #:syscall-error #:syscall-errno)
  (:import-from :sb-int #:stream-decoding-error)
  (:import-from :alexandria #:alist-hash-table #:hash-table-keys)
  (:import-from :sqlite #:connect #:execute-non-query #:execute-single #:sqlite-error #:sqlite-error-code)
  (:export #:with-indexing-thread))

(in-package :ballish/daemon/indexing)

(defvar *text-extensions-tags* (alist-hash-table '(("py" . ("python"))
						   ("cfg" . ("python"))
						   ("go" . ("golang")))
						 :test #'equal))

(defvar *text-extensions* (hash-table-keys *text-extensions-tags*))

(defvar *source-db* nil
  "The database used to store source code.")

(defvar *source-db-definitions*
  '("CREATE VIRTUAL TABLE IF NOT EXISTS source
     USING fts5(
         path UNINDEXED,
         content,
         tags
     )"
    ;; It may look odd to create a separate table, but during
    ;; indexing, we are doing a SELECT on path (to know if we need to
    ;; do an INSERT or an UPDATE), and doing that on the virtual table
    ;; takes >300ms, which means a super high overhead from
    ;; those. Doing the SELECT on a normally indexed field in a table
    ;; allows us to be 10x faster, even if we need to end up updating
    ;; 2 tables every time. Ideally we'd do an UPSERT on the virtual
    ;; table, but it's not supported because upserts only work if you
    ;; can manage to break an index constraint, and those aren't
    ;; supported on the virtual table.
    "CREATE TABLE IF NOT EXISTS source_meta(
         path TEXT PRIMARY KEY,
         mtime INTEGER NOT NULL
     )")
  "The table definitions for the sqlite tables.")

(defmacro with-indexing-thread ((queue) &body body)
  (let ((thread (gensym)))
    `(progn
       (setf *source-db* (connect (index-path #p"source.db")))
       (iter (for definition in *source-db-definitions*)
	     (execute-non-query *source-db* definition))
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
  (loop
     (handler-case
	 (return-from index-source
	   (let* ((path-str (namestring path))
		  (existing-mtime (execute-single *source-db*
						  "SELECT mtime FROM source_meta WHERE path = ?"
						  path-str)))
	     (if existing-mtime
		 (when (> mtime existing-mtime)
		   (execute-non-query *source-db*
				      "UPDATE source
                                       SET content = ?, tags = ?
                                       WHERE path = ?"
				      mtime
				      content
				      (format nil "~{~a~^ ~}" tags)
				      path-str)
		   (execute-non-query *source-db*
				      "UPDATE source_meta
                                       SET mtime = ?
                                       WHERE path = ?"
				      mtime
				      path-str))
		 (progn
		   (execute-non-query *source-db*
				      "INSERT INTO source(path, content, tags)
                                       VALUES(?, ?, ?)"
				      path-str
				      content
				      (format nil "~{~a~^ ~}" tags))
		   (execute-non-query *source-db*
				      "INSERT INTO source_meta(path, mtime)
                                       VALUES(?, ?)"
				      path-str
				      mtime)))))
       (sqlite-error (e)
	 (if (equal (sqlite-error-code e) :busy)
	     (sleep 0.1)
	     (error e))))))

(defun deindex-source (path)
  (let ((path-str (namestring path)))
    (execute-non-query *source-db* "DELETE FROM source WHERE path = ?" path-str)
    (execute-non-query *source-db* "DELETE FROM source_meta WHERE path = ?" path-str)))
