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

(defvar *folder* #p"/home/ralt/Dev/lab.plat.farm/foundation/foundation/")

(defun main ()
  (with-elasticsearch ()
    (let ((inotify (make-inotify)))
      (unwind-protect
	   (let ((files (make-mailbox)))
	     (add-watches inotify *folder* files)

	     (with-indexing-thread (files)
	       (format t "Ready to start watching~%")

	       (loop
		  (let* ((event (read-event inotify))
			 (mask (inotify-event-mask event)))
		    (cond ((and (member :create mask) (member :isdir mask))
			   (add-watches
			    inotify
			    (merge-pathnames
			     (make-pathname
			      :directory `(:relative ,(inotify-event-name event)))
			     (event-pathname/flags inotify event))
			    files)))
		    (format t "~a~%" event)))))
	(close-inotify inotify)))))

(defun add-watches (inotify path files)
  (watch inotify path '(:create :delete :delete-self :modify :move))

  (let ((wild-path (merge-pathnames (make-pathname :name :wild :type :wild) path)))
    (iter (for p in (directory wild-path))

	  (when (pathname-name p)
	    (send-message files p))

	  (when (and (not (pathname-name p))
		     (not (string= (first (last (pathname-directory p)))
				   ".git")))
	    (add-watches inotify p files)))))
