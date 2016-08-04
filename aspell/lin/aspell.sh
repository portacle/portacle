#!/bin/bash

readonly SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

export PATH=$ROOT/aspell/lin/bin:$PATH
export ASPELL_CONF="conf-dir $ROOT/aspell/share;home-dir $ROOT/config;data-dir $ROOT/aspell/lin/data"

"$ROOT/usr/lib/ld-linux.so" --library-path "$ROOT/usr/lib/" "$SCRIPT/bin/aspell" "$@"
