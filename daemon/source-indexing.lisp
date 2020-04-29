(uiop:define-package :ballish/daemon/source-indexing
    (:use :cl :iterate :ballish/util/*)
  (:import-from :sb-thread #:make-thread #:terminate-thread #:join-thread)
  (:import-from :sb-concurrency #:receive-message #:make-mailbox)
  (:import-from :sb-posix #:stat #:stat-mtime #:syscall-error #:syscall-errno)
  (:import-from :sb-int #:stream-decoding-error)
  (:import-from :alexandria #:alist-hash-table #:hash-table-keys)
  (:import-from :sqlite
		#:connect #:disconnect
		#:execute-non-query #:execute-single
		#:sqlite-error #:sqlite-error-code)
  (:export #:with-source-indexing
	   #:wait))

(in-package :ballish/daemon/source-indexing)

(defvar *text-extensions-tags* (alist-hash-table '(("py" . ("python"))
						   ("cfg" . ("python"))
						   ("go" . ("golang"))
						   ("lisp" . ("lisp"))
						   ("asd" . ("lisp"))
						   ("c" . ("c"))
						   ("h" . ("c" "c++" "header" "objective-c"))
						   ("cpp" . ("c++"))
						   ("pl" . ("perl" "prolog"))
						   ("php" . ("php"))
						   ("js" . ("javascript"))
						   ("gpr" . ("ada"))
						   ("adb" . ("ada"))
						   ("ads" . ("ada"))
						   ("agda" . ("agda"))
						   ("lagda" . ("agda"))
						   ("asm" . ("asm"))
						   ("bash" . ("sh"))
						   ("sh" . ("sh"))
						   ("cs" . ("csharp"))
						   ("clj" . ("clojure"))
						   ("cljs" . ("clojurescript"))
						   ("cbl" . ("cobol"))
						   ("cob" . ("cobol"))
						   ("cfm" . ("coldfusion"))
						   ("cfc" . ("coldfusion"))
						   ("d" . ("d"))
						   ("ex" . ("elixir"))
						   ("exs" . ("elixir"))
						   ("elm" . ("elm"))
						   ("el" . ("elisp"))
						   ("erl" . ("erlang"))
						   ("html" . ("html"))
						   ("java" . ("java"))
						   ("kt" . ("kotlin"))
						   ("kts" . ("kotlin"))
						   ("ktm" . ("kotlin"))
						   ("f90" . ("fortran"))
						   ("f95" . ("fortran"))
						   ("f03" . ("fortran"))
						   ("f" . ("fortran"))
						   ("for" . ("fortran"))
						   ("m" . ("objective-c"))
						   ("mm" . ("objective-c"))
						   ("M" . ("objective-c"))
						   ("ml" . ("ocaml"))
						   ("mli" . ("ocaml"))
						   ("pp" . ("pascal"))
						   ("pas" . ("pascal"))
						   ("inc" . ("pascal"))
						   ("sql" . ("sql"))
						   ("pro" . ("prolog"))
						   ("P" . ("prolog"))
						   ("rb" . ("ruby"))
						   ("rs" . ("rust"))
						   ("scm" . ("scheme"))
						   ("ss" . ("scheme"))
						   ("st" . ("smalltalk"))
						   ("swift" . ("swift"))
						   ("tcl" . ("tcl"))
						   ("tbc" . ("tcl"))
						   ("tex" . ("tex" "latex"))
						   ("ts" . ("typescript"))
						   ("tsx" . ("typescript"))
						   ("vim" . ("vimscript" "vim")))
						 :test #'equal))

(defvar *text-extensions* (hash-table-keys *text-extensions-tags*))

(defvar *table-definitions*
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

(defmacro with-source-indexing ((source-index queue) &body body)
  `(let* ((,queue (make-mailbox))
	  (,source-index (make-source-indexing ,queue)))
     (unwind-protect
	  (progn ,@body)
       (stop-indexing ,source-index))))

(defclass source-index ()
  ((thread :accessor thread)
   (files-queue :initarg :files-queue :reader files-queue)
   (db :initarg :db :reader db)))

(defmethod initialize-instance :after ((i source-index) &key)
  (iter (for definition in *table-definitions*)
	(execute-non-query (db i) definition))

  (setf (thread i) (make-thread #'index-loop
				:arguments (list i)
				:name "Source index")))

(defun make-source-indexing (files-queue)
  (make-instance 'source-index
		 :files-queue files-queue
		 :db (connect (source-index-db-path))))

(defun wait (index)
  (join-thread (thread index)))

(defun stop-indexing (index)
  (handler-case
      (terminate-thread (thread index))
    (error () nil))
  (disconnect (db index)))

(defun index-loop (index)
  (loop
     (destructuring-bind (action path)
	 (receive-message (files-queue index))
       (cond ((equal action :add)
	      (index-file index path))
	     ((equal action :delete)
	      (deindex-source index path))))))

(defun index-file (index path)
  (let ((path-type (pathname-type path)))
    (when (member path-type *text-extensions* :test #'string=)
      (handler-case
	  (let ((s (stat path)))
	    (index-source index
			  path
			  (stat-mtime s)
			  (uiop:read-file-string path)
			  (gethash path-type *text-extensions-tags*)))
	(syscall-error (e)
	  (when (= (syscall-errno e) 2)
	    (deindex-source index path)))
	(stream-decoding-error ()
	  ;; Ignore non-valid-utf-8 files
	  nil)))))

(defun index-source (index path mtime content tags)
  (loop
     (handler-case
	 (return-from index-source
	   (let* ((path-str (namestring path))
		  (existing-mtime (execute-single (db index)
						  "SELECT mtime FROM source_meta WHERE path = ?"
						  path-str)))
	     (if existing-mtime
		 (when (> mtime existing-mtime)
		   (execute-non-query (db index)
				      "UPDATE source
                                       SET content = ?, tags = ?
                                       WHERE path = ?"
				      content
				      (format nil "~{~a~^ ~}" tags)
				      path-str)
		   (execute-non-query (db index)
				      "UPDATE source_meta
                                       SET mtime = ?
                                       WHERE path = ?"
				      mtime
				      path-str))
		 (progn
		   (execute-non-query (db index)
				      "INSERT INTO source(path, content, tags)
                                       VALUES(?, ?, ?)"
				      path-str
				      content
				      (format nil "~{~a~^ ~}" tags))
		   (execute-non-query (db index)
				      "INSERT INTO source_meta(path, mtime)
                                       VALUES(?, ?)"
				      path-str
				      mtime)))))
       (sqlite-error (e)
	 (if (equal (sqlite-error-code e) :busy)
	     (sleep 0.1)
	     (error e))))))

(defun deindex-source (index path)
  (let ((path-str (namestring path)))
    (execute-non-query (db index) "DELETE FROM source WHERE path = ?" path-str)
    (execute-non-query (db index) "DELETE FROM source_meta WHERE path = ?" path-str)))
