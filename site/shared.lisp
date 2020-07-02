(uiop:define-package :ballish/site/shared
    (:use :cl)
  (:import-from #:hunchentoot :define-easy-handler :content-type*)
  (:import-from #:lass :*pretty* :compile-and-write)
  (:import-from #:spinneret :*html-style* :with-html-string)
  (:export #:with-page))

(in-package :ballish/site/shared)

(setf *html-style* :tree)
(setf *print-pretty* nil)
(setf *pretty* nil)

(define-easy-handler (style :uri "/style.css") ()
  (setf (content-type*) "text/css")
  (compile-and-write
   '(html
     :height 100%)
   '(body
     :margin 0
     :padding 0
     :height 100%
     :display grid
     :grid-template-rows auto 1fr auto)
   '(header
     :background (rgb 51 51 51)
     :display grid
     :grid-template-columns 50% 50%
     (.logo
      :margin 0
      :padding 5px
      :font-family "\"Lucida Console\", Monaco, monospace"
      :font-size 50px
      :color (rgb 220 0 0)
      (span.first
       :font-size 40px)
      (span.rest
       :font-size 30px
       :letter-spacing -5px))
     (.menu
      :display grid
      :grid-template-columns 20% 20% 20% 20% 20%
      :width 100%
      (a
       :color (rgb 255 255 250)
       :text-decoration none
       :justify-self center
       :align-self center)
      (a.selected
       :color (rgb 170 170 170))))
   '(section.body
     :margin 0 10%
     :padding 2% 1%
     :background (rgb 220 220 220))
   '(footer
     :background (rgb 51 51 51)
     :color (rgb 255 255 250)
     :padding 15px
     :font-size 85%
     :margin 0 10%
     :border-radius 0 0 5px 5px)))

(defmacro with-page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title " | Ballish, a pretty fast code search tool")
       (:link :rel "stylesheet" :href "/style.css"))
      (:body
       (:header
	(:div.logo
	 (:span.first "B")
	 (:span.rest "allish"))

	(:section.menu
	 (dolist (item '(("/" "Home")
			 ("https://gitlab.com/ralt/ballish/-/releases/" "Download")
			 ("https://gitlab.com/ralt/ballish/-/blob/master/MANUAL.md" "Manual")
			 ("https://gitlab.com/ralt/ballish/" "Contribute")
			 ("https://gitlab.com/ralt/ballish/-/issues/" "Contact")))
	   (:a :class (when (string= ,title (second item))
			"selected")
	       :href (first item)
	       (second item)))))
       (:section.body ,@body)
       (:footer "Copyright © 2020 Florian Margaine")))))
