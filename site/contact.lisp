(uiop:define-package :ballish/site/contact
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/contact)

(define-easy-handler (contact :uri "/contact.html") ()
  (with-page (:title "Contact")
    "In order to contact us for any issues, questions, or any reasons, it is best to use the " (:a :href "https://gitlab.com/ralt/ballish/-/issues/" "Gitlab issues") ". If it is a security issue, you can check the \"This issue is confidential\" checkbox."))
