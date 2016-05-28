#!/bin/bash
function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

readonly SCRIPT=$(dirname $(mreadlink "$0"))
export ROOT=${ROOT:-$(mreadlink "$SCRIPT/../../")/}

export PATH=$ROOT/git/mac/libexec/git-core:$PATH
export LD_LIBRARY_PATH=$ROOT/git/mac/bin/:$LD_LIBRARY_PATH
"$SCRIPT/bin/git" $@
