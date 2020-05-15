(defsystem "ballish"
  :license "GPLv2"
  :defsystem-depends-on ("wild-package-inferred-system" "cffi-grovel")
  :class "winfer:wild-package-inferred-system"
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
  (loop for object in sb-alien::*shared-objects*
     do (setf (sb-alien::shared-object-dont-save object) t))
  ;; TODO: figure out why (call-next-method op c) is erroring out?
  (setf uiop:*image-entry-point* (asdf/system:component-entry-point c)))
