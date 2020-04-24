(uiop:define-package :ballish/daemon/main
    (:use :cl :iterate :ballish/daemon/source-indexing)
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
  (let* ((source-queue (make-mailbox))
	 (source-indexing (make-source-indexing source-queue)))
    (iter (for folder in *folders*)
	  (index-folder folder source-queue))

    (unwind-protect
	 (wait source-indexing)
      (stop-indexing source-indexing))))

(defun index-folder (folder source-queue)
  (send-message source-queue folder)

  (let ((wild-path (merge-pathnames (make-pathname :directory '(:relative :wild)) folder)))
    (iter (for p in (directory wild-path))
	  (when (not (member (first (last (pathname-directory p)))
			     *ignored-folders*
			     :test #'string=))
	    (index-folder p source-queue)))))
