#!/usr/bin/env bash

# Some basic tests.

set -xe

tmp=$(mktemp -d)
trap "rm -rf $tmp" EXIT

export XDG_RUNTIME_DIR="$tmp"/runtime
export XDG_DATA_HOME="$tmp"/data

ballish-daemon &
trap "kill -9 %1" EXIT

sleep 1

bl -f fixtures

sleep 1

test $(bl -q foo | wc -l) = 1

test $(bl -t python | wc -l) = 2

test $(bl -q foo -t python | wc -l) = 1

test $(bl -q bar | wc -l) = 1

test $(bl -q baz | wc -l) = 0

test $(bl -q foo -g | wc -l) = 1
