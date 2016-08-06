#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

export PATH=$ROOT/git/lin/libexec/git-core:$PATH
export XDG_CONFIG_HOME=$ROOT/config

"$ROOT/usr/lib/ld-linux.so" --library-path "$ROOT/usr/lib/" "$SCRIPT/bin/git" "$@"
