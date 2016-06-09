#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

export PATH=$ROOT/git/lin/libexec/git-core:$PATH

"$ROOT/usr/lib/ld-linux.so" --library-path "$ROOT/usr/lib/" "$SCRIPT/bin/git" "$@"
