(uiop:define-package :ballish/site/manual
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/manual)

(define-easy-handler (manual :uri "/manual.html") ()
  (with-page (:title "Manual")
    "Hello!"))
