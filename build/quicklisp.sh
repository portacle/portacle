#!/bin/bash

readonly TAG=version-2016-01-21
readonly REPOSITORY=https://github.com/quicklisp/quicklisp-client.git

###

readonly PROGRAM=quicklisp
source common.sh
INSTALL_TARGET=$PORTACLE_DIR/$PROGRAM

case "$PLATFORM" in
    win) readonly SBCL="$PORTACLE_DIR/sbcl/win/sbcl.bat" ;;
    *)   readonly SBCL="$PORTACLE_DIR/sbcl/$PLATFORM/sbcl.sh" ;;
esac

function build() {
    "$SBCL" --script "$SOURCE_DIR/setup.lisp" \
        || eexit "Failed to set up quicklisp first-time init."
}

function install() {
    mkdir -p "$INSTALL_TARGET" \
        || eexit "Failed to create $INSTALL_TARGET"
    cp -Rf "$SOURCE_DIR/." "$INSTALL_TARGET" \
        || eexit "Failed to copy Quicklisp."
    rm -rf "$INSTALL_TARGET/.git"
}

main
