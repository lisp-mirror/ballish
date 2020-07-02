#!/usr/bin/env bash

# Make sure we act accordingly when the daemon is dead.

set -xe

. common.sh

set +e
bl -f fixtures
code=$?
set -e

test $code = 8

set +e
bl -q foo 2> log
code=$?
set -e

test $code = 8
test "$(cat log)" = "$(printf "fatal: no index found, are you sure the daemon is started?\n")"
