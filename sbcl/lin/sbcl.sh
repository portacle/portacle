#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}
export SBCL_HOME=$ROOT/sbcl/lin/lib/sbcl/

export LD_LIBRARY_PATH=$ROOT/usr/lib/:$LD_LIBRARY_PATH

"$SCRIPT/bin/sbcl" --no-sysinit --userinit "$ROOT/config/sbcl-init.lisp" "$@"
