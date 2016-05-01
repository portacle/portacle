#!/bin/sh
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f $SCRIPT/../../)/}
export SBCL_HOME=$ROOT/sbcl/lin/lib/sbcl/
./bin/sbcl --no-sysinit --userinit "$ROOT/.sbclrc" $@
