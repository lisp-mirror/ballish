#!/usr/bin/env bash

# Test that big files are not indexed.

set -e

. common.sh

set +x

line="I am trying to be at least to 40 characters"
for i in {0..300000}; do
    echo "$line" >> fixtures/big.sql
done
trap "rm -f fixtures/big.sql" EXIT

set -x

./tests/base.sh
