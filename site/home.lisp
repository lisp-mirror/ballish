(uiop:define-package :ballish/site/home
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/home)

(define-easy-handler (home :uri "/") ()
  (with-page (:title "Home")
    (:div "Hello!")))
