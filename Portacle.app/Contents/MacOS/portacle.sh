#!/bin/bash
function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

SCRIPT=$(dirname $(mreadlink "$0"))
export ROOT=$(mreadlink "$SCRIPT/../../../")/

"$ROOT/emacs/mac/emacs.sh"

