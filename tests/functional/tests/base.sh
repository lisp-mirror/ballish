#!/usr/bin/env bash

# Some basic tests.

set -xe

ballish-daemon &
trap "kill -9 %1" EXIT

sleep 1

bl -f fixtures

sleep 1

test $(bl -q trying | wc -l) = 0

test $(bl -q foo | wc -l) = 1

test $(bl -t python | wc -l) = 1

test $(bl -q foo -t python | wc -l) = 1

test $(bl -q bar | wc -l) = 1

test $(bl -q baz | wc -l) = 0

test $(bl -q foo -g | wc -l) = 1
