#!/bin/bash
function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

readonly SCRIPT=$(dirname $(mreadlink "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

ASPELL_SHARE="$ROOT/aspell/share"
export PATH="$ROOT/aspell/lin/bin:$PATH"
export ASPELL_CONF="conf-dir $ASPELL_SHARE;data-dir $ASPELL_SHARE;home-dir $ROOT/config"

"$SCRIPT/bin/aspell" "$@"
