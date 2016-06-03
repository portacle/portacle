#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}

export PATH=$ROOT/git/lin/libexec/git-core:$PATH
export LD_LIBRARY_PATH=$ROOT/usr/lib/:$LD_LIBRARY_PATH

"$SCRIPT/bin/git" $@
