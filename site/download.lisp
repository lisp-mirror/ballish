(uiop:define-package :ballish/site/download
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/download)

(defvar *version* (uiop:getenv "VERSION"))

(define-easy-handler (download :uri "/download.html") ()
  (with-page (:title "Download")
    (:article
     (:h2 "Packages")

     (:p (format
	  nil
	  "The packages for latest ballish release can be downloaded on Gitlab.com: "
	  *version*)
	 (:a :href (format
		    nil
		    "https://gitlab.com/ralt/ballish/-/releases/~a"
		    *version*)
	     (format nil "release ~a" *version*)))

     (:h2 "Installation instructions")

     (:ul
      (:li (:a :href "#arch" "Arch Linux"))
      (:li (:a :href "#debian" "Debian")
	   (:ul
	    (dolist (distribution '("stretch" "buster" "bullseye"))
	      (:li (:a :href (format nil "#debian-~a" distribution)
		       (format nil "Debian ~a" (string-capitalize distribution)))))))
      (:li (:a :href "#fedora" "Fedora")
	   (:ul
	    (dolist (version '("31" "32"))
	      (:li (:a :href (format nil "#fedora-~a" version)
		       (format nil "Fedora ~a" version))))))
      (:li (:a :href "#ubuntu" "Ubuntu")
	   (:ul
	    (dolist (version '("18.04" "19.10" "20.04"))
	      (:li (:a :href (format nil "#ubuntu-~a" version)
		       (format nil "Ubuntu ~a" version)))))))

     (flet ((instructions (file command)
	      (:p "Download the "
		  (:code file)
		  " file, then install the package with:")
	      (:pre (format nil "~a ~a" command file))
	      (:p "Then enable and start the daemon with:")
	      (:pre "sudo systemctl enable --now ballish-daemon@\"$USER\"")
	      (:p "You can then read the manual (either with " (:code "man ballish")
		  "or " (:a :href "/manual.html" "here") ")"
		  ", with the first command to look at being " (:code "bl --folder") ".")))

       (:div
	:id "arch"
	(:h3 "Arch Linux")

	(instructions (format nil "ballish-~a-1-x86_64.pkg.tar.xz" *version*)
		      "sudo pacman -U"))

       (:div
	:id "debian"
	(:h3 "Debian")

	(dolist (distribution '("stretch" "buster" "bullseye"))
	  (let ((file (format nil "ballish_~a_~a_amd64.deb" distribution *version*)))
	    (:div
	     :id (format nil "debian-~a" distribution)
	     (:h4 (format nil "Debian ~a" (string-capitalize distribution)))
	     (instructions file "sudo dpkg -i")))))

       (:div
	:id "fedora"
	(:h3 "Fedora")

	(dolist (version '("31" "32"))
	  (let ((file (format nil "ballish-~a-1.f~a.x86_64.rpm" *version* version)))
	    (:div
	     :id (format nil "fedora-~a" version)
	     (:h4 (format nil "Fedora ~a" version))
	     (instructions file "sudo dnf localinstall")))))

       (:div
	:id "ubuntu"
	(:h3 "Ubuntu")

	(dolist (version '("18.04" "19.10" "20.04"))
	  (let ((file (format nil "ballish_ubuntu_~a_~a_amd64.deb" version *version*)))
	    (:div
	     :id (format nil "ubuntu-~a" version)
	     (:h4 (format nil "Ubuntu ~a" version))
	     (instructions file "sudo dpkg -i")))))))))
