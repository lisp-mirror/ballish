#!/usr/bin/env bash

gem install --no-document fpm
export HOME=/home/lisp
git clone https://github.com/sbcl/sbcl.git ~/sbcl
# sbcl 2.0.4 is buggy for linkable-runtime
(cd ~/sbcl && git checkout sbcl-2.0.3 && sh make.sh --fancy --with-sb-linkable-runtime --with-sb-dynamic-core && sh install.sh)
curl -O https://beta.quicklisp.org/quicklisp.lisp && sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
git clone --depth=1 https://github.com/privet-kitty/wild-package-inferred-system.git ~/quicklisp/local-projects/wild-package-inferred-system/
git clone --depth=1 https://github.com/cffi/cffi.git ~/quicklisp/local-projects/cffi/
# upstream cl-inotify weakly depends on iolib, which is a PITA to bundle
git clone --depth=1 https://github.com/ralt/cl-inotify.git ~/quicklisp/local-projects/cl-inotify/
SBCL_HOME=/usr/local/lib/sbcl make
