(uiop:define-package :ballish/client/main
    (:use :cl :ballish/util/*)
  (:import-from :unix-opts
		#:define-opts
		#:get-opts
		#:option
		#:raw-arg
		#:unknown-option
		#:missing-arg
		#:arg-parser-failed
		#:missing-required-option)
  (:import-from :sqlite #:with-open-database #:execute-to-list #:execute-non-query)
  (:import-from :cl-ppcre #:split)
  (:import-from :sb-bsd-sockets
		#:socket
		#:local-socket
		#:socket-connect
		#:socket-close)
  (:export #:main))

(in-package :ballish/client/main)

(define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :query
   :description "run a query"
   :short #\q
   :long "query"
   :arg-parser #'identity
   :meta-var "QUERY")
  (:name :tags
   :description "tags for the query"
   :short #\t
   :long "tags"
   :arg-parser #'identity
   :meta-var "TAGS")
  (:name :folder
   :description "add folder to index"
   :short #\f
   :long "folder"
   :arg-parser #'identity
   :meta-var "FOLDER"))

(defun fatal (&rest args)
  (format *error-output* "fatal: ~a~%" (apply #'format nil args))
  (uiop:quit 1))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (multiple-value-bind (options args)
      (handler-case
	  (get-opts)
	(unknown-option (e)
	  (fatal "~s option is unknown" (option e)))
	(missing-arg (e)
	  (fatal "option ~s needs an argument" (option e)))
	(arg-parser-failed (e)
	  (fatal "cannot parse ~s as argument of ~s" (raw-arg e) (option e)))
	(missing-required-option (e)
	  (fatal "~a" e)))

    (when args
      (fatal "unknown parameters: ~{~a~^, ~}" args))

    (when-option (options :help)
      (opts:describe
       :prefix "a pretty fast code search tool"
       :usage-of "bl")
      (uiop:quit 0))

    (when (or (getf options :query)
	      (getf options :tags))
      (return-from main (query (getf options :query) (getf options :tags))))

    (when-option (options :folder)
      (return-from main (add-folder (getf options :folder))))))

(defun query (q tags)
  (with-open-database (db (source-index-db-path) :busy-timeout 1000)
    (let ((query
	   (format
	    nil
	    "SELECT path FROM source WHERE ~a ~a"
	    (if q (format nil "content MATCH ~s" q) "")
	    (if tags
		(format nil "~a ~{tags MATCH ~s~^AND ~}"
			(if q "AND" "")
			(split "," tags))
		""))))
      (format t "~{~a~%~}" (mapcar #'car (execute-to-list db query))))))

(defun add-folder (folder)
  (with-open-database (db (ballish-db-path) :busy-timeout 1000)
    (execute-non-query db "INSERT INTO folder (path) VALUES(?)" folder)

    (let ((socket (make-instance 'local-socket :type :stream)))
      (unwind-protect
	   (socket-connect
	    socket
	    (namestring (uiop:xdg-runtime-dir #p"ballish/daemon.sock")))
	(socket-close socket)))))
