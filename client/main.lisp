(uiop:define-package :ballish/client/main
    (:use :cl :iterate :ballish/util/*)
  (:import-from :alexandria #:lastcar)
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
                #:sqlite-constraint-error
                #:sqlite-error
                #:sqlite-error-code
                #:sqlite-error-message)
  (:import-from :cl-ppcre #:split #:regex-replace-all)
  (:import-from :sb-thread #:make-thread #:join-thread)
  (:import-from :sb-concurrency #:make-mailbox #:receive-message #:send-message)
  (:import-from :sb-bsd-sockets
                #:socket
                #:local-socket
                #:socket-connect
                #:socket-close
                #:socket-error
                #:socket-send
                #:socket-receive)
  (:import-from :sb-posix #:stat #:stat-size #:syscall-error)
  (:export #:main))

(in-package :ballish/client/main)

(uiop:register-image-dump-hook (before-dump-hook 'sqlite-ffi::sqlite3-lib))

(defvar *version* (uiop:getenv "VERSION"))

(defvar *debug* nil
  "Debug mode.")

(defvar *repositories-toplevel-markers*
  '(".git" ".svn" ".hg"))

(define-condition fatal-error (error)
  ((message :initarg :message :initform "" :reader message)
   (code :initarg :code :initform 1 :reader code)
   (condition :initarg :condition :initform nil :reader fatal-error-condition))
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

(defun find-repository-toplevel (folder)
  (when (= (length (pathname-directory folder)) 1)
    (error 'fatal-error
           :message "unable to find the root folder of the repository"
           :code 6))

  (dolist (path (directory (make-pathname :directory (pathname-directory folder)
                                          :name :wild :type :wild)))
    (let ((name (pathname-name path)))
      (if name
          (when (member name *repositories-toplevel-markers* :test #'string=)
            (return-from find-repository-toplevel folder))
          (if (member (lastcar (pathname-directory path))
                      *repositories-toplevel-markers*
                      :test #'string=)
              (return-from find-repository-toplevel folder)))))

  (find-repository-toplevel (make-pathname :directory (butlast (pathname-directory folder)))))

(defun searched-folder (options)
  (when-option (options :localized)
    (return-from searched-folder (getf options :localized)))
  (when-option (options :repository)
    (return-from searched-folder
      (namestring (find-repository-toplevel (uiop:getcwd))))))

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
   :arg-parser #'normalize-folder
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
   :long "optimize")
  (:name :debug
   :description "run in debug mode"
   :short #\d
   :long "debug")
  (:name :status
   :description "show indexing status"
   :short #\s
   :long "status")
  (:name :version
   :description "print version"
   :short #\v
   :long "version")
  (:name :localized
   :description "search in a localized folder"
   :short #\l
   :long "localized"
   :arg-parser #'normalize-folder
   :meta-var "FOLDER")
  (:name :repository
   :description "search in the current repository"
   :short #\r
   :long "repository"))

(defun main ()
  (handler-bind* ((error (lambda (c)
                           (when *debug*
                             (format *error-output* "~a~%" c))
                           (format *error-output* "fatal: unhandled error~%")
                           (uiop:quit -1)))
                  (fatal-error (lambda (c)
                                 (when (and *debug* (fatal-error-condition c))
                                   (format *error-output* "~a~%" (fatal-error-condition c)))
                                (format *error-output* "~a" c)
                                (uiop:quit (code c))))
                  (sb-sys:interactive-interrupt
                   (lambda (c)
                     (declare (ignore c))
                     (error 'fatal-error
                            :message "program interrupted by user"
                            ;; 1 is reserved for (fatal)
                            :code 2)))
                  (socket-error (lambda (c)
                                  (error 'fatal-error
                                         :message "ballish-daemon is not started."
                                         :code 3
                                         :condition c)))
                  (sqlite-error (lambda (c)
                                  (if (eql (sqlite-error-code c) :busy)
                                      (error 'fatal-error
                                             :message "please try again later"
                                             :code 4
                                             :condition c)
                                      (error 'fatal-error
                                             :message "unhandled sqlite error"
                                             :code 5
                                             :condition c)))))
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

      (setf *debug* (getf options :debug))

      (when-option (options :version)
        (return-from main (format t "version: ~a~%" *version*)))

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
        (return-from main
          (format t "~{~a~%~}"
                  (query-count (getf options :query)
                               (getf options :tags)
                               (searched-folder options)))))

      (when (or (getf options :query)
                (getf options :tags))
        (return-from main
          (let ((results (query (getf options :query)
                                (getf options :tags)
                                nil
                                (searched-folder options))))
            (if (getf options :grep)
                (grep (getf options :query) results)
                (format t "~{~a~%~}" results)))))

      (when-option (options :folder)
        (return-from main
          (add-folder (getf options :folder))))

      (when-option (options :status)
        (return-from main
          (show-status)))

      (when (or (getf options :repository)
                (getf options :localized))
        (error 'fatal-error
               :message "--repository and --localized need to be run with a query"
               :code 7)))))

(defun query (q tags &optional (count nil) (path nil))
  (with-open-database (db (source-index-db-path) :busy-timeout 1000)
    (let ((query
           (format
            nil
            "SELECT ~a FROM source ~a ~a ~a ~a"
            (if count "count(*)" "path")
            (if (or q tags) "WHERE" "")
            (if q (format nil "source MATCH 'content:~a'" q) "")
            (if tags
                (format nil "~a ~{source MATCH 'tags:~a'~^ AND ~}"
                        (if q "AND" "")
                        (split "," tags))
                "")
            (if path
                (format nil "AND path MATCH 'path:^~a'"
                        (regex-replace-all
                         "\\."
                         (format nil "~{~a~^+~}" (rest (split "/" path)))
                         "+"))
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
           (progn
             (socket-connect
              socket
              (namestring (ballish-daemon-socket-path)))
             (socket-send socket "rfsh" nil))
        (socket-close socket)))))

(defun query-count (q tags path)
  (query q tags t path))

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
  (with-open-database (db (source-index-db-path) :busy-timeout 100000)
    (execute-non-query db "INSERT INTO source(source) VALUES('optimize')")))

(defun show-status ()
  (let* ((socket (test-server))
         (index-size (index-size))
         (queue-count (when socket (queue-count socket)))
         (folders-list (list-folders)))
    (format t "server status: ~a~%index size on disk: ~$M~%~aindexed folders:~%~a~%"
            (if socket "up" "down")
            index-size
            (if socket
                (format nil "in-flight files to index: ~a~%"
                        (if (eql queue-count :busy)
                            "more than 1000"
                            queue-count))
                "")
            (format nil "~{  - ~a~^~%~}" folders-list))))

(defun test-server ()
  ;; fun fact: this one returns an open socket.
  (let ((socket (make-instance 'local-socket :type :stream)))
    (handler-case
        (progn
          (socket-connect
           socket
           (namestring (ballish-daemon-socket-path)))
          socket)
      (socket-error ()
        nil))))

(defun index-size ()
  (handler-case
      (/ (stat-size (stat (source-index-db-path))) 1024.0 1024.0)
    (syscall-error ()
      0)))

(defun queue-count (socket)
  ;; fun fact: this one closes the socket.
  (unwind-protect
       (progn
         (socket-send socket "qcnt" nil )
         (let ((recv-thread
                (make-thread
                 (lambda (socket)
                   (multiple-value-bind (response length)
                       (socket-receive socket nil 32)
                     (parse-integer (subseq (the simple-string response) 0 length))))
                 :arguments (list socket))))
           (join-thread recv-thread :default :busy :timeout 1)))
    (socket-close socket)))

(defun list-folders ()
  (with-open-database (db (ballish-db-path) :busy-timeout 1000)
    (mapcar #'car (execute-to-list db "SELECT path FROM folder"))))
