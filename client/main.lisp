(uiop:define-package :ballish/client/main
    (:use :cl)
  (:import-from :unix-opts
		#:define-opts
		#:get-opts
		#:option
		#:raw-arg
		#:unknown-option
		#:missing-arg
		#:arg-parser-failed
		#:missing-required-option)
  (:export #:main))

(in-package :ballish/client/main)

(define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose"))

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
 
    (when-option (options :help)
      (opts:describe
       :prefix "blazing-fast code source search"
       :usage-of "bl"
       :args "[QUERY|FOLDER]")
      (uiop:quit 0))

    (when-option (options :verbose)
      (format t "Running in verbose mode.~%"))

    (unless args
      (fatal "invalid argument"))

    (format t "Args: ~a~%" args)))
