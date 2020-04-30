all: ballish-daemon bl ballish.1.gz

ballish-daemon: $(wildcard daemon/*.lisp)
	sbcl \
		--load ~/quicklisp/setup.lisp \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :ballish)' \
		--eval '(asdf:make :ballish)'

bl: $(wildcard client/*.lisp)
	sbcl \
		--load ~/quicklisp/setup.lisp \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :ballish/client)' \
		--eval '(asdf:make :ballish/client)'

ballish.1.gz: MANUAL.md
	pandoc -s -t man $< | gzip -9 > $@

.PHONY: release

release:
	fpm -s dir -t deb --license GPLv2 --description "A pretty fast code search tool" --maintainer "Florian Margaine <florian@margaine.com>" -n ballish -v $(VERSION) bl=/usr/bin/ ballish-daemon=/usr/bin/ ballish.1.gz=/usr/share/man/man1/
	fpm -s dir -t rpm --license GPLv2 --description "A pretty fast code search tool" --maintainer "Florian Margaine <florian@margaine.com>" -n ballish -v $(VERSION) bl=/usr/bin/ ballish-daemon=/usr/bin/ ballish.1.gz=/usr/share/man/man1/
	fpm -s dir -t pacman --license GPLv2 --description "A pretty fast code search tool" --maintainer "Florian Margaine <florian@margaine.com>" -n ballish -v $(VERSION) bl=/usr/bin/ ballish-daemon=/usr/bin/ ballish.1.gz=/usr/share/man/man1/
