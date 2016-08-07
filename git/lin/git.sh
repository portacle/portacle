#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

export PATH=$ROOT/git/lin/libexec/git-core:$PATH
export XDG_CONFIG_HOME=$ROOT/config
export LW_LOADER_PATH="$ROOT/usr/lib/ld-linux.so"
export LW_LIBRARY_PATH="$ROOT/usr/lib/"
export LD_PRELOAD="$ROOT/usr/lib/ld-wrap.so"

"$LW_LOADER_PATH" --library-path "$LW_LIBRARY_PATH" "$SCRIPT/bin/git" "$@"
