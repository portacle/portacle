#!/bin/bash

readonly SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

ASPELL_SHARE="$ROOT/aspell/share"
export PATH="$ROOT/aspell/lin/bin:$PATH"
export ASPELL_CONF="conf-dir $ASPELL_SHARE;data-dir $ASPELL_SHARE;home-dir $ROOT/config"

"$ROOT/usr/lib/ld-linux.so" --library-path "$ROOT/usr/lib/" "$SCRIPT/bin/aspell" "$@"
