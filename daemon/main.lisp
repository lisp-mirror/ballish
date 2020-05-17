(uiop:define-package :ballish/daemon/main
    (:use :cl :iterate :ballish/daemon/source-indexing :ballish/util/*)
  (:import-from :sb-concurrency #:make-mailbox #:send-message #:receive-message)
  (:import-from :sb-thread #:make-thread #:terminate-thread)
  (:import-from :cl-inotify
		#:with-inotify
		#:read-event
		#:inotify-event
		#:watch
		#:unwatch
		#:inotify-event-mask
		#:inotify-event-name
		#:event-pathname/flags)
  (:import-from :sqlite #:with-open-database #:execute-non-query #:execute-to-list)
  (:import-from :sb-bsd-sockets
		#:socket
		#:local-socket
		#:socket-bind
		#:socket-listen
		#:socket-close
		#:socket-accept)
  (:import-from :sb-posix #:getuid)
  (:import-from :log4cl #:log-debug #:log-info)
  (:export #:main))

(in-package :ballish/daemon/main)

(defvar *table-definitions*
  '("CREATE TABLE IF NOT EXISTS folder(
         path TEXT PRIMARY KEY
     )"))

(defvar *ignored-folders* '(".git" ".svn" ".hg" ".tox" "__pycache__"))

(defmacro with-waiting-thread ((fn &key arguments name) &body body)
  (let ((thread (gensym)))
    `(let ((,thread (make-thread #',fn :arguments ,arguments :name ,name)))
       (unwind-protect
            (progn ,@body)
         (handler-case
             (terminate-thread ,thread)
           (error () nil))))))

(defmacro with-waiting-threads (thread-definitions &body body)
  (expand-waiting-threads thread-definitions body))

(eval-when (:compile-toplevel)
  (defun expand-waiting-threads (thread-definitions body)
    (if (= (length thread-definitions) 0)
	`(progn ,@body)
	`(with-waiting-thread ,(first thread-definitions)
	   ,(expand-waiting-threads (rest thread-definitions) body)))))

(defmacro with-ballish-database ((db) &body body)
  (let ((definition (gensym)))
    `(with-open-database (,db (ballish-db-path))
       (iter (for ,definition in *table-definitions*)
	     (execute-non-query ,db ,definition))
       ,@body)))

(defmacro with-unix-socket-server ((socket &key path (backlog 100)) &body body)
  `(progn
     (handler-case
	 (delete-file ,path)
       (file-error () nil))
     (let ((,socket (make-instance 'local-socket
				   :type :stream)))
       (socket-bind ,socket ,path)
       (socket-listen ,socket ,backlog)
       (unwind-protect
	    (progn ,@body)
	 (socket-close ,socket)))))

(defmacro with-ballish-server ((socket) &body body)
  `(progn
     (ensure-directories-exist (uiop:xdg-runtime-dir #p"ballish/"))
     (with-unix-socket-server (,socket
			       :path ,(namestring
				       (uiop:xdg-runtime-dir #p"ballish/daemon.sock")))
       ,@body)))

(defun main ()
  (when (not (= 0 (getuid)))
    (let ((max-user-watches
	   (parse-integer
	    (uiop:read-file-string #p"/proc/sys/fs/inotify/max_user_watches"))))
      (when (< max-user-watches 100000)
	(format
	 *error-output*
	 "fs.inotify.max_user_watches is less than 100,000, you should increase that for a better experience.~%"))))

  (with-inotify (inotify)
    (with-ballish-database (db)
      (with-source-indexing (source-index source-queue)
	(let ((folders (get-folders db)))
	  (when folders (log-info "Adding watches on folders ~a" folders))
	  (iter (for folder in folders)
		(add-watches inotify folder source-queue))

	  (if folders
	      (log-info "Watches added, entering main loop now")
	      (log-info "Entering main loop"))

	  (with-ballish-server (socket)
	    (main-loop db inotify socket source-index source-queue folders)))))))

(defun main-loop (db inotify socket source-index source-queue folders)
  (let ((queue (make-mailbox)))
    (with-waiting-threads ((wait-for-indexing
			    :arguments (list source-index)
			    :name "Main source index wait indexing")
			   (wait-for-inotify-event
			    :arguments (list inotify queue)
			    :name "Main wait inotify")
			   (wait-for-client-socket
			    :arguments (list socket queue)
			    :name "Main wait client socket"))
      (loop
	 (let ((message (receive-message queue)))
	   (log-debug "Received message ~a" message)
	   (typecase message
	     (inotify-event
	      (handle-inotify-event inotify message source-queue))

	     (socket
	      (setf folders
		    (handle-client-socket message db inotify source-queue folders)))))))))

(defun add-watches (inotify path source-queue &optional (action :add))
  ;; TODO: don't watch on read-only files (either fs or permission)
  (log-debug "Watching ~a with action ~a" path action)
  (handler-case
      (cond
	((equal action :add)
	 (watch inotify path '(:create :delete :delete-self :modify :move)))
	((equal action :delete)
	 (unwatch inotify :pathname path)))
    (osicat-posix:posix-error ()
      ;; Ignore. It typically means we're reaching the
      ;; max_user_watches limit.
      nil))

  (let ((wild-path (merge-pathnames (make-pathname :name :wild :type :wild) path)))
    (iter (for p in (directory wild-path :resolve-symlinks nil))
	  (when (pathname-name p)
	    (send-message source-queue (list action p)))

	  (when (and (not (pathname-name p))
		     (not (member (first (last (pathname-directory p)))
				  *ignored-folders*
				  :test #'string=)))
	    (add-watches inotify p source-queue)))))

(defun remove-watches (inotify path source-queue)
  (add-watches inotify path source-queue :delete))

(defun wait-for-indexing (source-indexing)
  (wait source-indexing))

(defun wait-for-inotify-event (inotify queue)
  (loop
     (send-message queue (read-event inotify))))

(defun wait-for-client-socket (socket queue)
  (loop
     (send-message queue (socket-accept socket))))

(defun handle-inotify-event (inotify event source-queue)
  (let* ((mask (inotify-event-mask event))
	 (path (event-pathname/flags inotify event)))
    (log-debug "Got event ~a for path ~a" event path)
    (cond (;; New folder
	   (and (member :create mask) (member :isdir mask))
	   (add-watches
	    inotify
	    (merge-pathnames
	     (make-pathname :directory `(:relative ,(inotify-event-name event)))
	     path)
	    source-queue))

	  ;; File created/modified/deleted
	  ((and (not (member :isdir mask))
		(or (member :create mask)
		    (member :modify mask)
		    (member :delete mask)
		    (member :delete-self mask)
		    (member :move-to mask)))
	   (send-message source-queue (list :add path)))

	  ((and (not (member :isdir mask))
		(member :move-from mask))
	   (send-message source-queue (list :delete path))))))

(defun get-folders (db)
  (mapcar #'car (execute-to-list db "SELECT path FROM folder")))

(defun handle-client-socket (socket db inotify source-queue folders)
  (unwind-protect
       (let* ((new-folders (get-folders db))
	      (folders-to-watch (set-difference new-folders folders :test #'equal))
	      (folders-to-delete (set-difference folders new-folders :test #'equal)))
	 (iter (for folder in folders-to-watch)
	       (add-watches inotify folder source-queue))

	 (iter (for folder in folders-to-delete)
	       (remove-watches inotify folder source-queue))

	 new-folders)
    (socket-close socket)))
