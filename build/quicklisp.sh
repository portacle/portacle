#!/bin/bash

readonly TAG=version-2016-01-21
readonly REPOSITORY=https://github.com/quicklisp/quicklisp-client.git

###

readonly PROGRAM=quicklisp
source common.sh
INSTALL_DIR=$PORTACLE_DIR/all/$PROGRAM

function build() {
    "$SHARED_BIN_DIR/sbcl" --script "$SOURCE_DIR/setup.lisp" \
        || eexit "Failed to set up quicklisp first-time init."
}

function install() {
    mkdir -p "$INSTALL_DIR" \
        || eexit "Failed to create $INSTALL_DIR"
    cp -Rf "$SOURCE_DIR/." "$INSTALL_DIR" \
        || eexit "Failed to copy Quicklisp."
    rm -rf "$INSTALL_DIR/.git"
}

main
