(defsystem "ballish"
  :license "GPLv2"
  :defsystem-depends-on ("wild-package-inferred-system" "cffi-grovel")
  :class "winfer:wild-package-inferred-system"
  :around-compile (lambda (next)
                    (proclaim '(optimize
				(debug 3)
				(safety 1)
				(speed 3)))
                    (funcall next))
  :depends-on ("lparallel" "ballish/daemon/*")
  :components ((:c-file "sqlite3" :cflags ("-DSQLITE_ENABLE_FTS5"
					   "-DSQLITE_DQS=0"
					   "-DSQLITE_DEFAULT_MEMSTATUS=0"
					   "-DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1"
					   "-DSQLITE_LIKE_DOESNT_MATCH_BLOBS"
					   "-DSQLITE_MAX_EXPR_DEPTH=0"
					   "-DSQLITE_OMIT_DECLTYPE"
					   "-DSQLITE_OMIT_DEPRECATED"
					   "-DSQLITE_OMIT_PROGRESS_CALLBACK"
					   "-DSQLITE_OMIT_SHARED_CACHE"
					   "-DSQLITE_USE_ALLOCA"
					   "-DHAVE_FDATASYNC")))
  :build-operation :static-program-op
  :build-pathname "ballish-daemon"
  :entry-point "ballish/daemon/main:main")

(defsystem "ballish/client"
  :defsystem-depends-on ("wild-package-inferred-system" "cffi-grovel")
  :class "winfer:wild-package-inferred-system"
  :around-compile (lambda (next)
                    (proclaim '(optimize
				(debug 3)
				(safety 1)
				(speed 3)))
                    (funcall next))
  :depends-on ("ballish"
	       "ballish/client/*")
  :in-order-to ((test-op (test-op "ballish/client/tests")))
  :build-operation :static-program-op
  :build-pathname "bl"
  :entry-point "ballish/client/main:main")

(defsystem "ballish/client/tests"
  :defsystem-depends-on ("wild-package-inferred-system" "cffi-grovel")
  :class "winfer:wild-package-inferred-system"
  :around-compile (lambda (next)
                    (proclaim '(optimize
				(debug 3)
				(safety 1)
				(speed 3)))
                    (funcall next))
  :depends-on ("ballish/client"
	       "ballish/tests/unit/client/*")
  :perform (test-op (o c) (when (and (not (symbol-call :fiveam '#:run-all-tests))
				     (uiop:getenv "CI"))
			    (uiop:quit -1))))

(defmethod perform :before ((op program-op) (c system))
  ;; Close the non-system foreign libraries as we're using
  ;; :static-program-op to embed them into our binary. The system
  ;; libraries cannot be embedded, though, so we need to keep them.
  (uiop:register-image-dump-hook
   (lambda ()
     (dolist (library (cffi:list-foreign-libraries))
       (when (eql (cffi:foreign-library-type library) :grovel-wrapper)
	 (cffi:close-foreign-library library)))))
  (setf uiop:*image-entry-point* (asdf/system:component-entry-point c)))
