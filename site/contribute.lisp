(uiop:define-package :ballish/site/contribute
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/contribute)

(define-easy-handler (contribute :uri "/contribute.html") ()
  (with-page (:title "Contribute")
    (:p "Thank you for wanting to contribute!")
    (:p "We do not have a formal Code of Conduct, but we expect potential contributors to remain professional.")
    (:p "The development primarily happens on " (:a :href "https://gitlab.com/ralt/ballish" "Gitlab") " over the Merge Request workflow, but the current maintainer is open to more distributed approaches, such as sending patches or pulling from remote repositories.")
    (:p "Ballish is a Common Lisp project, even this website is written in Lisp. If you wish to contribute, we gladly welcome merge requests! Note, however, that we may not necessarily accept them.")
    (:p "In order to build ballish yourself, given the model of distribution that it is using, the typical simple workflow does not work out of the box, and a couple of custom repositories are required. That said, before anything, you should familiarize yourself, and make sure those are installed, with:")

    (:ul
     (:li (:a :href "http://sbcl.org" "sbcl"))
     (:li (:a :href "https://www.quicklisp.org/beta/" "QuickLisp"))
     (:li (:a :href "https://pandoc.org/" "pandoc")))

    (:p "You will need to compile sbcl manually, because a necessary compilation option isn't enabled by default: make sure you compile it with " (:code "sh make.sh --fancy --with-sb-linkable-runtime"))
    (:blockquote :class "small" "Note: sbcl 2.0.4 has a known bug preventing ballish compilation. Versions before and after are fine.")

    (:p "You will then need to clone those dependencies in your " (:code "~/common-lisp/") "folder:")

    (:ul
     (:li (:a :href "https://github.com/privet-kitty/wild-package-inferred-system" "wild-package-inferred-system"))
     (:li (:a :href "https://github.com/cffi/cffi" "cffi") " (unreleased fixes are necessary)")
     (:li (:a :href "https://github.com/ralt/cl-inotify" "cl-inotify") " (fork to avoid a bad dependency)"))

    (:p "Once you have all of this, you can run " (:code "make") ", after which the binaries should appear in your folder.")

    (:p "There are 2 kind of tests to run: unit tests and integration tests. Unit tests are run with " (:code "make tests") ", whilst integration tests are run with " (:code "cd tests/functional && ./run-tests") ".")

    (:p "In addition, if you want to build a package (Arch, Debian, Fedora, Ubuntu), your best bet is looking at the appropriate job definition in the " (:a :href "https://gitlab.com/ralt/ballish/-/blob/master/.gitlab-ci.yml" ".gitlab-ci.yml") ". Each distribution has its own small set of packages to install, after which a generic build script can run, packaging is then done with " (:a :href "https://github.com/jordansissel/fpm/" "fpm") " (aka \"Effin Package Management\") in the Makefile.")))
