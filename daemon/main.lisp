(uiop:define-package :ballish/daemon/main
    (:use :cl :ballish/daemon/elasticsearch :iterate :ballish/daemon/indexing)
  (:import-from :cl-inotify
		#:make-inotify
		#:close-inotify
		#:read-event
		#:watch
		#:inotify-event-mask
		#:inotify-event-name
		#:event-pathname/flags)
  (:import-from :sb-concurrency #:make-mailbox #:send-message)
  (:export #:main))

(in-package :ballish/daemon/main)

(defvar *folders* '(#p"/home/ralt/Dev/lab.plat.farm/"
		    #p"/home/ralt/.venvs/foundation/lib/python2.7/site-packages/"
		    #p"/home/ralt/.venvs/git/lib/python2.7/site-packages/"
		    #p"/home/ralt/go/src/"
		    #p"/home/ralt/go/pkg/mod/"))

(defvar *ignored-folders* '(".git" ".svn" ".hg" ".tox" "__pycache__"))

(defun main ()
  (with-elasticsearch ()
    (let ((inotify (make-inotify)))
      (unwind-protect
	   (let ((files (make-mailbox)))
	     (with-indexing-thread (files)
	       (iter (for folder in *folders*)
		     (add-watches inotify folder files))

	       (loop
		  (let* ((event (read-event inotify))
			 (mask (inotify-event-mask event)))
		    (cond (;; New folder/file
			   (and (member :create mask) (member :isdir mask))
			   (add-watches
			    inotify
			    (merge-pathnames
			     (make-pathname
			      :directory `(:relative ,(inotify-event-name event)))
			     (event-pathname/flags inotify event))
			    files))

			  ;; file changed
			  ((and (member :modify mask) (not (member :isdir mask)))
			   (send-message files (event-pathname/flags inotify event)))

			  ;; file deleted
			  ((and (or (member :delete mask) (member :delete-self mask))
				(not (member :isdir mask)))
			   (send-message files (event-pathname/flags inotify event))))))))
	(close-inotify inotify)))))

(defun add-watches (inotify path files)
  (handler-case
      (watch inotify path '(:create :delete :delete-self :modify :move))
    (osicat-posix:posix-error ()
      ;; silently ignore. It can be many reasons, and we just don't
      ;; really care, it means we're skipping.
      (return-from add-watches)))

  (let ((wild-path (merge-pathnames (make-pathname :name :wild :type :wild) path)))
    (iter (for p in (directory wild-path))

	  (when (pathname-name p)
	    (send-message files p))

	  (when (and (not (pathname-name p))
		     (not (member (first (last (pathname-directory p)))
				  *ignored-folders*
				  :test #'string=)))
	    (add-watches inotify p files)))))
