#!/usr/bin/env bash

set -e # exit on error
set -u # error on undefined var
set -o pipefail # exit on command pipe failure

# Set working dir to script location
cd "$(dirname "$0")"

# Hardcoded mapping of source to destination files
declare -A file_map
file_map["../docs/src/Readme.lhs"]="../README.md"

for from in "${!file_map[@]}"; do
    to="${file_map[$from]}"
    echo "Converting $from -> $to"
    pandoc \
        --standalone \
        "$from" \
        -o "$to" \
        --from markdown+lhs --to gfm
done
