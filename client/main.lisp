(uiop:define-package :ballish/client/main
    (:use :cl :iterate :ballish/util/*)
  (:import-from :unix-opts
                #:define-opts
                #:get-opts
                #:option
                #:raw-arg
                #:unknown-option
                #:missing-arg
                #:arg-parser-failed
                #:missing-required-option)
  (:import-from :cl-cpus #:get-number-of-processors)
  (:import-from :sqlite
                #:with-open-database
                #:execute-to-list
                #:execute-non-query
                #:sqlite-constraint-error)
  (:import-from :cl-ppcre #:split #:regex-replace-all)
  (:import-from :sb-thread #:make-thread #:join-thread)
  (:import-from :sb-concurrency #:make-mailbox #:receive-message #:send-message)
  (:import-from :sb-bsd-sockets
                #:socket
                #:local-socket
                #:socket-connect
                #:socket-close
                #:socket-error)
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
   :meta-var "FOLDER")
  (:name :count
   :description "count the results of a query"
   :short #\c
   :long "count")
  (:name :grep
   :description "show grep results for a query"
   :short #\g
   :long "grep")
  (:name :optimize
   :description "optimize the search index storage"
   :short #\o
   :long "optimize"))

(define-condition fatal-error (error)
  ((message :initarg :message :initform "" :reader message)
   (code :initarg :code :initform 1 :reader code))
  (:report (lambda (condition stream)
             (format stream "fatal: ~a~%" (message condition)))))

(defun fatal (&rest args)
  (error 'fatal-error :message (format nil "~a" (apply #'format nil args))))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defmacro handler-bind* (bindings &body body)
  (expand-handler-binds bindings body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-handler-binds (bindings body)
    (let ((binding (first bindings)))
      (if binding
          `(handler-bind (,binding)
             ,(expand-handler-binds (rest bindings) body))
          `(progn ,@body)))))

(defun normalize-folder (path)
  (check-type path string)
  (concatenate
   'string
   (string-right-trim
    '(#\. #\/)
    (the
     simple-string
     (uiop:unix-namestring (uiop:merge-pathnames* (uiop:parse-unix-namestring path)))))
   "/"))

(defun main ()
  (handler-bind* ((fatal-error (lambda (c)
                                (format *error-output* "~a" c)
                                (uiop:quit (code c))))
		  (sb-sys:interactive-interrupt
		   (lambda (c)
		     (declare (ignore c))
		     (error 'fatal-error
			    :message "program interrupted by user"
			    :code 1)))
		  (socket-error (lambda (c)
				  (declare (ignore c))
				  (error 'fatal-error
					 :message "ballish-daemon is not started."
					 :code 2))))
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

      (when-option (options :optimize)
	(return-from main
	  (progn
	    (format t "Optimizing storage...~%")
	    (optimize-fts))))

      (when-option (options :count)
	(if (or (getf options :query)
		(getf options :tags))
	    (return-from main
	      (format t "~{~a~%~}"
		      (query-count (getf options :query)
				   (getf options :tags))))
	    (fatal "cannot count without a query")))

      (when (or (getf options :query)
		(getf options :tags))
	(return-from main
	  (let ((results (query (getf options :query) (getf options :tags))))
	    (if (getf options :grep)
		(grep (getf options :query) results)
		(format t "~{~a~%~}" results)))))

      (when-option (options :folder)
	(return-from main
	  (add-folder (normalize-folder (getf options :folder))))))))

(defun query (q tags &optional (count nil))
  (with-open-database (db (source-index-db-path) :busy-timeout 1000)
    (let ((query
           (format
            nil
            "SELECT ~a FROM source WHERE ~a ~a"
            (if count "count(*)" "path")
            (if q (format nil "source MATCH 'content:~a'" q) "")
            (if tags
                (format nil "~a ~{source MATCH 'tags:~s'~^ AND ~}"
                        (if q "AND" "")
                        (split "," tags))
                ""))))
      (mapcar #'car (execute-to-list db query)))))

(defun add-folder (folder)
  (with-open-database (db (ballish-db-path) :busy-timeout 1000)
    (handler-case
        (execute-non-query db "INSERT INTO folder (path) VALUES(?)" folder)
      (sqlite-constraint-error ()
        (fatal "folder ~a is already indexed." folder)))

    (let ((socket (make-instance 'local-socket :type :stream)))
      (unwind-protect
           (socket-connect
            socket
            (namestring (ballish-daemon-socket-path)))
        (socket-close socket)))))

(defun query-count (q tags)
  (query q tags t))

(defun grep (query results)
  (when (> (length results) (or (and (uiop:getenv "BL_MAX_GREP_RESULTS")
                                     (parse-integer (uiop:getenv "BL_MAX_GREP_RESULTS")))
                                100))
    (fatal "too many results to grep."))
  (let ((search (regex-replace-all "(\\w+)"
                                   (regex-replace-all "[\\+\\s]+" query ".*")
                                   "\\b\\1\\b")))
    (handler-case
        (uiop:run-program
         (format nil "grep -HPins ~s ~{~s~^ ~}" search results)
         :output :interactive :error-output :interactive :input :interactive)
      (error (e) (format *error-output* "~a~%" e)))))

(defun optimize-fts ()
  (with-open-database (db (source-index-db-path) :busy-timeout 10000)
    (execute-non-query db "INSERT INTO source(source) VALUES('optimize')")))
