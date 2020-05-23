#!/usr/bin/env bash

# Some basic tests.

set -xe

. common.sh

ballish-daemon &
trap "kill -9 %1" EXIT

sleep 1

bl -f fixtures

sleep 1

test $(bl -q trying | wc -l) = 0

test $(bl -q foo | wc -l) = 2

test $(bl -t python | wc -l) = 3

test $(bl -q foo -t python | wc -l) = 2

test $(bl -q bar | wc -l) = 1

test $(bl -q baz | wc -l) = 0

test $(bl -q foo -g | wc -l) = 3

test $(bl -c) = 3

(cd fixtures/subfolder && test $(bl -q qux -r | wc -l) = 2)

test $(bl -q qux -l fixtures/subfolder | wc -l) = 2

set +e
bl -q foo -r
code=$?
set -e

test $code = 6

set +e
bl -r
code=$?
set -e

test $code = 7

# we don't have 'set -o pipefail' so the command succeeds.
test $(bl -q 2>&1 | wc -l) = 1
