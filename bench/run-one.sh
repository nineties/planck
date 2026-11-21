#!/bin/bash
set -u
tree=$1
planck0=$2
case_name=$3
bench=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp)
err=$(mktemp)
timing=$(mktemp)
cd "$tree" || exit 2
/usr/bin/time -f "%e %M" -o "$timing" "$planck0" "$bench/cases/$case_name.pk" > "$out" 2> "$err"
status=$?
if [ $status -ne 0 ]; then
    echo "$case_name: planck0 exited with $status" >&2
    cat "$err" >&2
    exit 1
fi
if ! diff -u "$bench/cases/$case_name.out" "$out" >&2; then
    echo "$case_name: output differs from the expected output" >&2
    exit 1
fi
tail -1 "$timing"
