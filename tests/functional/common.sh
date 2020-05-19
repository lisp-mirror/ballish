#!/usr/bin/env bash

set -e

export PATH=$(realpath ../..):$PATH

tmp=$(mktemp -d)
trap "rm -rf $tmp" EXIT

export XDG_RUNTIME_DIR="$tmp"/runtime
export XDG_DATA_HOME="$tmp"/data

cp -R fixtures "$tmp"
cp -R tests "$tmp"
cp common.sh "$tmp"

cd "$tmp"
