(uiop:define-package :ballish/daemon/main
    (:use :cl :iterate :ballish/daemon/source-indexing)
  (:import-from :sb-concurrency #:make-mailbox #:send-message #:receive-message)
  (:import-from :sb-thread #:make-thread #:terminate-thread)
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
  (:export #:main))

(in-package :ballish/daemon/main)

(defvar *folders* '(#p"/home/ralt/Dev/lab.plat.farm/"
		    #p"/home/ralt/.venvs/foundation/lib/python2.7/site-packages/"
		    #p"/home/ralt/.venvs/git/lib/python2.7/site-packages/"
		    #p"/home/ralt/go/src/"
		    #p"/home/ralt/go/pkg/mod/"))

(defvar *ignored-folders* '(".git" ".svn" ".hg" ".tox" "__pycache__"))

(defun main ()
  (let* ((source-queue (make-mailbox))
	 (source-indexing (make-source-indexing source-queue))
	 (inotify (make-inotify)))
    (iter (for folder in *folders*)
	  (add-watches inotify folder source-queue))

    (unwind-protect
	 (let* ((queue (make-mailbox))
		(source-index-thread (make-thread #'wait-for-indexing
						  :arguments (list source-indexing queue)
						  :name "Main source index wait"))
		(inotify-event-thread (make-thread #'wait-for-inotify-event
						   :arguments (list inotify queue))))
	   (unwind-protect
		(loop
		   (let ((message (receive-message queue)))
		     (typecase message
		       (inotify-event
			(handle-inotify-event inotify message source-queue)))))
	     (terminate-thread source-index-thread)
	     (terminate-thread inotify-event-thread)))
      (stop-indexing source-indexing))))

(defun add-watches (inotify path source-queue)
  ;; TODO: don't watch on read-only files (either fs or permission)
  (handler-case
      (watch inotify path '(:create :delete :delete-self :modify :move))
    (posix-error (e)
      ;; Ignore. It can be many reasons, and we just don't really
      ;; care, it means we're skipping.
      (format *error-output* "Error watching ~a: ~a~%" path e)
      (return-from add-watches)))

  (let ((wild-path (merge-pathnames (make-pathname :name :wild :type :wild) path)))
    (iter (for p in (directory wild-path))
	  (when (pathname-name p)
	    (send-message source-queue p))

	  (when (and (not (pathname-name p))
		     (not (member (first (last (pathname-directory p)))
				  *ignored-folders*
				  :test #'string=)))
	    (add-watches inotify p source-queue)))))

(defun wait-for-indexing (source-indexing queue)
  (send-message queue (wait source-indexing)))

(defun wait-for-inotify-event (inotify queue)
  (loop
     (send-message queue (read-event inotify))))

(defun handle-inotify-event (inotify event source-queue)
  (let* ((mask (inotify-event-mask event))
	 (path (event-pathname/flags inotify event)))
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
		    (member :delete-self mask)))
	   (send-message source-queue path))

	  ;; TODO: add :move-from/:move-to
	  )))
