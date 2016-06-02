#!/bin/bash
function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

SCRIPT=$(dirname $(mreadlink "$0"))
export ROOT=${ROOT:-$(mreadlink "$SCRIPT/../../")/}
export SBCL_HOME=$ROOT/sbcl/mac/lib/sbcl/

export DYLD_LIBRARY_PATH=$ROOT/shared/lib/:$DYLD_LIBRARY_PATH

"$SCRIPT/bin/sbcl" --no-sysinit --userinit "$ROOT/config/sbcl-init.lisp" $@
