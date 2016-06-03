#!/bin/bash
function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

readonly SCRIPT=$(dirname $(mreadlink "$0"))
export ROOT=${ROOT:-$(mreadlink "$SCRIPT/../../")/}

export PATH=$ROOT/git/mac/libexec/git-core:$PATH
export DYLD_LIBRARY_PATH=$ROOT/usr/lib/:$DYLD_LIBRARY_PATH

"$SCRIPT/bin/git" $@
