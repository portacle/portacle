#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}
export SBCL_HOME=$ROOT/sbcl/lin/lib/sbcl/

"$ROOT/usr/lib/ld-linux.so" --library-path "$ROOT/usr/lib/" "$SCRIPT/bin/sbcl" --no-sysinit --userinit "$ROOT/config/sbcl-init.lisp" "$@"
