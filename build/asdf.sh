#!/bin/bash

readonly TAG=3.1.7.5
readonly REPOSITORY=https://gitlab.common-lisp.net/asdf/asdf.git

###

readonly PROGRAM=asdf
source common.sh
INSTALL_TARGET=$PORTACLE_DIR/$PROGRAM

function build() {
    cd "$SOURCE_DIR"
    make
    cd "$SOURCE_DIR/build/"
    cat >compile.lisp <<EOF
(cl:compile-file "asdf.lisp" :output-file "asdf.fasl")
(sb-ext:exit)
EOF
    case "$PLATFORM" in
        win) winroot=$({ cd "$SCRIPT_DIR/.." && pwd -W; } | sed 's|/|\\|g')
             cmd /c "$winroot\\sbcl\\win\\sbcl.bat --script compile.lisp" ;;
        *)   $SCRIPT_DIR/../sbcl/$PLATFORM/sbcl.sh --script compile.lisp;;
    esac
}

function install() {
    ensure-installed "$INSTALL_TARGET" "$SOURCE_DIR/build/asdf.lisp" "$SOURCE_DIR/build/asdf.fasl" \
        || eexit "Failed to copy ASDF sources."
}

main
