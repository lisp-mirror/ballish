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

.PHONY: deb rpm pkg tests client-tests

tests: client-tests

client-tests: $(wildcard tests/unit/client/*.lisp)
	sbcl \
		--load ~/quicklisp/setup.lisp \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :cffi-grovel)' \
		--eval '(ql:quickload :ballish/client/tests)' \
		--eval '(asdf:test-system :ballish/client)' \
		--quit

common_args = --license GPLv2 --description "A pretty fast code search tool" --maintainer "Florian Margaine <florian@margaine.com>" --name ballish --version $(VERSION) bl=/usr/bin/ ballish-daemon=/usr/bin/ ballish.1.gz=/usr/share/man/man1/ 50-ballish.conf=/usr/lib/sysctl.d/ emacs/ballish.el=/usr/share/emacs/site-lisp/ vim/ballish.vim=/usr/share/vim/vimfiles/plugin/ballish.vim vim/ballish.vim=/usr/share/nvim/site/plugin/ballish.vim
most_args = $(common_args) ballish-daemon@.service=/lib/systemd/system/
arch_args = $(common_args) ballish-daemon@.service=/usr/lib/systemd/system/

deb:
	fpm -s dir -t deb $(most_args)

rpm:
	fpm -s dir -t rpm $(most_args)

pkg:
	fpm -s dir -t pacman $(arch_args)
