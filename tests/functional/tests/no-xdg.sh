#!/usr/bin/env bash

# Test that the daemon and the client still work together even if no
# XDG_* environment variables are set.

set -xe

tmp=$(mktemp -d)
trap "rm -rf $tmp" EXIT

export HOME=$tmp

unset XDG_RUNTIME_DIR
unset XDG_DATA_HOME

ballish-daemon &
trap "kill -9 %1" EXIT

sleep 1

bl -f fixtures

sleep 1

test $(bl -q foo | wc -l) = 1
