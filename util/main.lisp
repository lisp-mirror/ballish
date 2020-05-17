(uiop:define-package :ballish/util/main
    (:use :cl)
  (:import-from :sb-posix #:getuid)
  (:export #:ballish-db-path
	   #:source-index-db-path
	   #:ballish-daemon-socket-path))

(in-package :ballish/util/main)

(defun ballish-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun ballish-db-path ()
  (let ((path (ballish-path #p"ballish.db")))
    (ensure-directories-exist path)
    path))

(defun ballish-runtime-path (&rest more)
  ;; UIOP doesn't resolve if $XDG_RUNTIME_DIR isn't defined.
  (uiop:resolve-absolute-location
   (list (or (uiop:xdg-runtime-dir)
	     (merge-pathnames (write-to-string (getuid)) #p"/run/user/"))
	 #p"ballish/"
	 more)))

(defun ballish-daemon-socket-path ()
  (let ((path (ballish-runtime-path #p"daemon.sock")))
    (ensure-directories-exist path)
    path))

(defun source-index-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun source-index-db-path ()
  (let ((path (source-index-path #p"source.db")))
    (ensure-directories-exist path)
    path))
