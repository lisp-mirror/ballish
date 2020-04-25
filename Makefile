all: ballish-daemon ballish-client

ballish-daemon: $(wildcard daemon/*.lisp)
	sbcl --eval '(push "$(PWD)/" asdf:*central-registry*)' --eval '(asdf:make :ballish)'

ballish-client: $(wildcard client/*.lisp)
	sbcl --eval '(push "$(PWD)/" asdf:*central-registry*)' --eval '(asdf:make :ballish/client)'
