#!/bin/bash
SCRIPT=$(dirname $(readlink -f "$0"))
export ROOT=${ROOT:-$(readlink -f "$SCRIPT/../../")/}
export SBCL_HOME=$ROOT/sbcl/lin/lib/sbcl/

## The following does not work because ???
## ld-linux.so just seems to sweep past the sbcl binary argument
## for reasons that are completely beyond me. I cannot figure out
## how to fix that behaviour either, so we revert to using system
## provided libs and hoping things work out.
# "$ROOT/usr/lib/ld-linux.so" --library-path "$ROOT/usr/lib/" "$SCRIPT/bin/sbcl" --no-sysinit --userinit "$ROOT/config/sbcl-init.lisp" "$@"

"$SCRIPT/bin/sbcl" --no-sysinit --userinit "$ROOT/config/sbcl-init.lisp" "$@"
