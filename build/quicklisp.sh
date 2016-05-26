#!/bin/bash

readonly TAG=version-2016-01-21
readonly REPOSITORY=https://github.com/quicklisp/quicklisp-client.git

###

PLATFORM="."
readonly PROGRAM=quicklisp
source common.sh

function build() {
    sbcl --script "$SOURCE_DIR/setup.lisp"
}

function install() {
    mkdir -p "$INSTALL_TARGET"
    cp -R "$SOURCE_DIR/." "$INSTALL_TARGET"
    rm -rf "$INSTALL_TARGET/.git"
}

main
