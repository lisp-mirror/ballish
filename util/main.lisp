(uiop:define-package :ballish/util/main
    (:use :cl)
  (:export #:ballish-db-path
	   #:source-index-db-path))

(in-package :ballish/util/main)

(defun ballish-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun ballish-db-path ()
  (let ((path (ballish-path #p"ballish.db")))
    (ensure-directories-exist path)
    path))

(defun source-index-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun source-index-db-path ()
  (let ((path (source-index-path #p"source.db")))
    (ensure-directories-exist path)
    path))
