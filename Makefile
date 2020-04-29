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
