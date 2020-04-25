all: ballish-daemon ballish-client

ballish-daemon: $(wildcard daemon/*.lisp)
	sbcl --eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' --eval '(push "$(PWD)/" asdf:*central-registry*)' --eval '(asdf:make :ballish)'

ballish-client: $(wildcard client/*.lisp)
	sbcl --eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' --eval '(push "$(PWD)/" asdf:*central-registry*)' --eval '(asdf:make :ballish/client)'
