(uiop:define-package :ballish/util/main
    (:use :cl)
  (:export #:ballish-db-path
	   #:source-index-db-path))

(in-package :ballish/daemon/main)

(defun ballish-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun ballish-db-path ()
  (ballish-path #p"ballish.db"))

(defun source-index-path (&rest more)
  (uiop:xdg-data-home #p"ballish/" more))

(defun source-index-db-path ()
  (source-index-path #p"source.db"))
