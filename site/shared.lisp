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
     :grid-template-columns 25% 25% 50%
     (.logo
      :margin 0
      :padding 5px
      :font-family "\"Lucida Console\", Monaco, monospace"
      :font-size 50px
      :color (rgb 265 165 0)
      (span.first
       :font-size 40px)
      (span.rest
       :font-size 30px
       :letter-spacing -5px))
     (.subtitle
      :color (rgb 255 255 250)
      :justify-self center
      :align-self center)
     (.menu
      :display grid
      :grid-template-columns 20% 20% 20% 20% 20%
      :width 100%
      (.item
       :color (rgb 255 255 250)
       :text-decoration none
       :justify-self center
       :align-self center)
      (.item.selected
       :color (rgb 170 170 170))))
   '(section.body
     :margin 0 20%
     :padding 2% 1%
     :font-size 110%
     (.small
      :font-size 85%)
     (article
      :display grid)
     (pre
      :background (rgb 51 51 51)
      :color (rgb 238 238 238)
      :padding 10px
      :width 100%
      :overflow auto
      :font-size 110%)
     (img
      :width 70%)
     (hr
      :width 100%
      :border 1px solid black
      :margin 5% 0))
   '(:media "(max-width: 1000px)"
     (section.body
      :margin 0
      ((and div :target)
       :background (rgb 240 240 240)
       :padding 10px)
      (pre
       :width 95%)
      (img
       :width 95%)))
   '(footer
     :background (rgb 51 51 51)
     :color (rgb 255 255 250)
     :padding 15px
     :font-size 85%
     :margin 0)))

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

	(:div.subtitle
	 "A pretty fast code search tool")

	(:section.menu
	 (dolist (item '(("/" "About")
			 ("/download.html" "Download")
			 ("/manual.html" "Manual")
			 ("https://gitlab.com/ralt/ballish/" "Contribute")
			 ("https://gitlab.com/ralt/ballish/-/issues/" "Contact")))
	   (if (string= ,title (second item))
	       (:span :class "selected item" (second item))
	       (:a :class "item" :href (first item) (second item))))))
       (:section.body ,@body)
       (:footer "Copyright © 2020 Florian Margaine")))))
