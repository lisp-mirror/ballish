#!/usr/bin/env bash

set -xe

export VERSION="${CI_BUILD_TAG:-1.0.0}"
gem install --no-document fpm &> /dev/null
export HOME=/home/lisp

git clone https://github.com/sbcl/sbcl.git ~/sbcl &> /dev/null
# sbcl 2.0.4 is buggy for linkable-runtime
(
    set -e
    cd ~/sbcl
    git checkout sbcl-2.0.3 &> /dev/null

    set +e
    sh make.sh --fancy --with-sb-linkable-runtime --with-sb-dynamic-core &> sbcl-build.log
    code=$?
    set -e
    test $code = 0 || (cat sbcl-build.log && exit 1)

    sh install.sh &> /dev/null
)
curl -O https://beta.quicklisp.org/quicklisp.lisp && sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit &> /dev/null
git clone --depth=1 https://github.com/privet-kitty/wild-package-inferred-system.git ~/quicklisp/local-projects/wild-package-inferred-system/ &> /dev/null

# Unreleased fixes upstream
git clone --depth=1 https://github.com/cffi/cffi.git ~/quicklisp/local-projects/cffi/

# upstream cl-inotify weakly depends on iolib, which is a PITA to bundle
git clone --depth=1 https://github.com/ralt/cl-inotify.git ~/quicklisp/local-projects/cl-inotify/ &> /dev/null

set +e
SBCL_HOME=/usr/local/lib/sbcl make &> ballish-build.log
code=$?
set -e
test $code = 0 || (cat ballish-build.log && exit 1)

echo "Running unit tests..."
set +e
CI=1 make tests &> make-tests.log
code=$?
set -e
test $code = 0 || (cat make-tests.log && exit 1)

# We need to do this after "make tests"
rm -rf ~/.cache/common-lisp

echo "Running functional tests..."
(cd tests/functional; ./run-tests)
