(uiop:define-package :ballish/daemon/source-indexing
    (:use :cl :iterate)
  (:import-from :sb-thread #:make-thread #:terminate-thread #:join-thread)
  (:import-from :sb-concurrency
		#:make-mailbox #:send-message #:receive-message
		#:make-gate #:open-gate #:wait-on-gate)
  (:import-from :sb-posix #:stat #:stat-mtime #:syscall-error #:syscall-errno)
  (:import-from :sb-int #:stream-decoding-error)
  (:import-from :alexandria #:alist-hash-table #:hash-table-keys)
  (:import-from :sqlite
		#:connect #:disconnect
		#:execute-non-query #:execute-single
		#:sqlite-error #:sqlite-error-code)
  (:import-from :cl-inotify
		#:make-inotify
		#:close-inotify
		#:read-event
		#:inotify-event
		#:watch
		#:inotify-event-mask
		#:inotify-event-name
		#:event-pathname/flags)
  (:import-from :osicat-posix #:posix-error)
  (:export #:with-indexing-thread
	   #:make-source-indexing
	   #:wait
	   #:stop-indexing))

(in-package :ballish/daemon/source-indexing)

(defvar *text-extensions-tags* (alist-hash-table '(("py" . ("python"))
						   ("cfg" . ("python"))
						   ("go" . ("golang")))
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

(defclass source-index ()
  ((thread :accessor thread)
   (folder-queue :initarg :folder-queue :reader folder-queue)
   (db :initarg :db :reader db)
   (inotify :initarg :inotify :reader inotify)
   (stop-gate :initarg :stop-gate :reader stop-gate)))

(defmethod initialize-instance :after ((i source-index) &key)
  (iter (for definition in *table-definitions*)
	(execute-non-query (db i) definition))

  (setf (thread i) (make-thread #'index-loop
				:arguments (list i)
				:name "Source index")))

(defun make-source-indexing (folder-queue)
  (make-instance 'source-index
		 :folder-queue folder-queue
		 :inotify (make-inotify)
		 :stop-gate (make-gate :name "Source index stop")
		 :db (connect (index-path #p"source.db"))))

(defun wait (index)
  (join-thread (thread index)))

(defun stop-indexing (index)
  (open-gate (stop-gate index))
  (join-thread (thread index))
  (close-inotify (inotify index))
  (disconnect (db index)))

(defun index-loop (index)
  (let* ((queue (make-mailbox :name "Source index"))
	 (folder-thread (make-thread #'wait-for-new-folders
				     :arguments (list index queue)
				     :name "Source index wait for new folders"))
	 (inotify-thread (make-thread #'wait-for-new-inotify-events
				      :arguments (list index queue)
				      :name "Source index wait for new inotify events"))
	 (stop-gate-thread (make-thread #'wait-for-stop-gate
					:arguments (list index queue)
					:name "Source index wait for stop gate")))
    (unwind-protect
	 (loop
	    (let ((message (receive-message queue)))
	      (typecase message
		(pathname
		 (watch-and-index-folder index message))

		(inotify-event
		 (handle-inotify-event index message))

		(t (return-from index-loop)))))
      (terminate-thread folder-thread)
      (terminate-thread inotify-thread)
      (terminate-thread stop-gate-thread))))

(defun wait-for-new-folders (index queue)
  (loop
     (send-message queue (receive-message (folder-queue index)))))

(defun wait-for-new-inotify-events (index queue)
  (loop
     (send-message queue (read-event (inotify index)))))

(defun wait-for-stop-gate (index queue)
  (wait-on-gate (stop-gate index))
  (send-message queue t))

(defun index-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun watch-and-index-folder (index folder)
  (add-watch (inotify index) folder)
  (index-folder index folder))

(defun add-watch (inotify folder)
  ;; TODO: don't watch on read-only files (either fs or permission)
  (handler-case
      (watch inotify folder '(:create :delete :delete-self :modify :move))
    (posix-error (e)
      ;; Ignore. It can be many reasons, and we just don't really
      ;; care, it means we're skipping.
      (format *error-output* "Error watching ~a: ~a~%" folder e))))

(defun index-folder (index folder)
  (let ((wild-path (merge-pathnames (make-pathname :name :wild :type :wild) folder)))
    (iter (for path in (directory wild-path))
	  (when (pathname-name path)
	    (index-file index path)))))

(defun handle-inotify-event (index event)
  (let ((mask (inotify-event-mask event))
	(path (event-pathname/flags (inotify index) event)))
    (cond (;; New folder
	   (and (member :create mask) (member :isdir mask))
	   (watch-and-index-folder
	    index
	    (merge-pathnames
	     (make-pathname :directory `(:relative ,(inotify-event-name event)))
	     path)))

	  ;; New file
	  ((and (member :create mask) (not (member :isdir mask)))
	   (index-file index path))

	  ;; File changed
	  ((and (member :modify mask) (not (member :isdir mask)))
	   (index-file index path))

	  ;; File deleted
	  ((and (or (member :delete mask) (member :delete-self mask))
		(not (member :isdir mask)))
	   (deindex-source index path))

	  ;; TODO: add :move-from/:move-to
	  )))

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
				      mtime
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
