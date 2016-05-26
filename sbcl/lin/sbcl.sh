#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}
export SBCL_HOME=$ROOT/sbcl/lin/lib/sbcl/
"$SCRIPT/bin/sbcl" --no-sysinit --userinit "$ROOT/.sbclrc" $@
