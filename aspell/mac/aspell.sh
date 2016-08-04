#!/bin/bash
function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

readonly SCRIPT=$(dirname $(mreadlink "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

export PATH=$ROOT/aspell/lin/bin:$PATH
export ASPELL_CONF="conf-dir $ROOT/aspell/share;home-dir $ROOT/config;data-dir $ROOT/aspell/mac/data"

"$SCRIPT/bin/aspell" "$@"
