#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}
export PATH=$ROOT/git/lin/libexec/git-core:$PATH
"$SCRIPT/bin/git" $@
