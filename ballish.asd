(defsystem "ballish"
  :license "GPLv2"
  :defsystem-depends-on ("wild-package-inferred-system" "cffi-grovel")
  :class "winfer:wild-package-inferred-system"
  :around-compile (lambda (next)
                    (proclaim '(optimize
				(debug 1)
				(safety 1)
				(debug 1)
				(speed 3)))
                    (funcall next))
  :depends-on ("ballish/daemon/*")
  :build-operation :static-program-op
  :build-pathname "ballish-daemon"
  :entry-point "ballish/daemon/main:main")

(defsystem "ballish/client"
  :defsystem-depends-on ("wild-package-inferred-system" "cffi-grovel")
  :class "winfer:wild-package-inferred-system"
  :around-compile (lambda (next)
                    (proclaim '(optimize
				(debug 1)
				(safety 1)
				(debug 1)
				(speed 3)))
                    (funcall next))
  :depends-on ("ballish"
	       "ballish/client/*")
  :build-operation :static-program-op
  :build-pathname "bl"
  :entry-point "ballish/client/main:main")

(defmethod perform :before ((op program-op) (c system))
  ;; Close the non-system foreign libraries as we're using
  ;; :static-program-op to embed them into our binary. The system
  ;; libraries cannot be embedded, though, so we need to keep them.
  (uiop:register-image-dump-hook
   (lambda ()
     (loop
	for library in (cffi:list-foreign-libraries)
	unless (eq (cffi:foreign-library-type library) :system)
	do (cffi:close-foreign-library library))))
  (setf uiop:*image-entry-point* (asdf/system:component-entry-point c)))
