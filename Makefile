all: ballish-daemon bl ballish.1.gz

ballish-daemon: $(wildcard daemon/*.lisp)
	sbcl \
		--load ~/quicklisp/setup.lisp \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :cffi-grovel)' \
		--eval '(ql:quickload :ballish)' \
		--eval '(asdf:make :ballish)' \
		--quit

bl: $(wildcard client/*.lisp)
	sbcl \
		--load ~/quicklisp/setup.lisp \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :cffi-grovel)' \
		--eval '(ql:quickload :ballish/client)' \
		--eval '(asdf:make :ballish/client)' \
		--quit

ballish.1.gz: MANUAL.md
	pandoc -s -t man $< | gzip -9 > $@

.PHONY: deb rpm pkg

deb:
	fpm -s dir -t deb --depends libsqlite3-0 --license GPLv2 --description "A pretty fast code search tool" --maintainer "Florian Margaine <florian@margaine.com>" -n ballish -v $(VERSION) bl=/usr/bin/ ballish-daemon=/usr/bin/ ballish.1.gz=/usr/share/man/man1/ ballish-daemon.service=/lib/systemd/system/

rpm:
	fpm -s dir -t rpm --depends sqlite-devel --license GPLv2 --description "A pretty fast code search tool" --maintainer "Florian Margaine <florian@margaine.com>" -n ballish -v $(VERSION) bl=/usr/bin/ ballish-daemon=/usr/bin/ ballish.1.gz=/usr/share/man/man1/ ballish-daemon.service=/lib/systemd/system/

pkg:
	fpm -s dir -t pacman --depends sqlite --license GPLv2 --description "A pretty fast code search tool" --maintainer "Florian Margaine <florian@margaine.com>" -n ballish -v $(VERSION) bl=/usr/bin/ ballish-daemon=/usr/bin/ ballish.1.gz=/usr/share/man/man1/ ballish-daemon.service=/lib/systemd/system/
