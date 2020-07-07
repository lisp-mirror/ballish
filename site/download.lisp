(uiop:define-package :ballish/site/download
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/download)

(defvar *version* (uiop:getenv "VERSION"))

(define-easy-handler (download :uri "/download") ()
  (with-page (:title "Download")
    (:article
     (:h2 "Packages")

     (:p (format
	  nil
	  "The packages for ballish ~a can be downloaded on Gitlab.com: "
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
      (:li (:a :href "#ubuntu" "Ubuntu")
	   (:ul
	    (dolist (version '("18.04" "19.10" "20.04"))
	      (:li (:a :href (format nil "#ubuntu-~a" version)
		       (format nil "Ubuntu ~a" version))))))
      (:li (:a :href "#fedora" "Fedora")
	   (:ul
	    (dolist (version '("31" "32"))
	      (:li (:a :href (format nil "#fedora-~a" version)
		       (format nil "Fedora ~a" version)))))))

     (:h3 :id "arch" "Arch Linux")

     (:p "Download the "
	 (:code (format nil "ballish-~a-1-x86_64.pkg.tar.xz" *version*))
	 " file, then install the package with:")
     (:pre (format nil "sudo pacman -U ballish-~a-1-x86_64.pkg.tar.xz" *version*))
     (:p "Then enable and start the daemon with:")
     (:pre "sudo systemctl enable --now ballish-daemon@\"$USER\"")

     (:h3 :id "debian" "Debian")

     (dolist (distribution '("stretch" "buster" "bullseye"))
       (let ((file (format nil "ballish_~a_~a_amd64.deb" distribution *version*)))
	 (:h4 :id (format nil "debian-~a" distribution)
	      (format nil "Debian ~a" (string-capitalize distribution)))
	 (:p "Download the "
	     (:code file)
	     " file, then install the package with:")
	 (:pre (format nil "sudo dpkg -i ~a" file))
	 (:p "Then enable and start the daemon with:")
	 (:pre "sudo systemctl enable --now ballish-daemon@\"$USER\"")))

     (:h3 :id "ubuntu" "Ubuntu")

     (dolist (version '("18.04" "19.10" "20.04"))
       (let ((file (format nil "ballish_ubuntu_~a_~a_amd64.deb" version *version*)))
	 (:h4 :id (format nil "ubuntu-~a" version) (format nil "Ubuntu ~a" version))
	 (:p "Download the "
	     (:code file)
	     " file, then install the package with:")
	 (:pre (format nil "sudo dpkg -i ~a" file))
	 (:p "Then enable and start the daemon with:")
	 (:pre "sudo systemctl enable --now ballish-daemon@\"$USER\"")))

     (:h3 :id "fedora" "Fedora")

     (dolist (version '("31" "32"))
       (let ((file (format nil "ballish-~a-1.f~a.x86_64.rpm" *version* version)))
	 (:h4 :id (format nil "fedora-~a" version) (format nil "Fedora ~a" version))
	 (:p "Download the "
	     (:code file)
	     " file, then install the package with:")
	 (:pre (format nil "sudo dnf localinstall ~a" file))
	 (:p "Then enable and start the daemon with:")
	 (:pre "sudo systemctl enable --now ballish-daemon@\"$USER\""))))))
