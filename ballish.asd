(defsystem "ballish"
  :license "GPLv2"
  :defsystem-depends-on ("wild-package-inferred-system")
  :class "winfer:wild-package-inferred-system"
  :depends-on ("ballish/daemon/*")
  :build-operation program-op
  :build-pathname "ballish-daemon"
  :entry-point "ballish/daemon/main:main")

(defsystem "ballish/client"
  :defsystem-depends-on ("wild-package-inferred-system")
  :class "winfer:wild-package-inferred-system"
  :depends-on ("ballish"
	       "ballish/client/*")
  :build-operation program-op
  :build-pathname "bl"
  :entry-point "ballish/client/main:main")
