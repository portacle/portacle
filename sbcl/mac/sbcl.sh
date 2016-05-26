#!/bin/sh
function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

SCRIPT=$(dirname $(mreadlink "$0"))
export ROOT=${ROOT:-$(mreadlink "$SCRIPT/../../")/}
export SBCL_HOME=$ROOT/sbcl/mac/lib/sbcl/
"$SCRIPT/bin/sbcl" --no-sysinit --userinit "$ROOT/.sbclrc" $@
