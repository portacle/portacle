#!/bin/bash

readonly TAG=
readonly REPOSITORY=https://github.com/shinmera/portacle-launcher.git

###

readonly PROGRAM=launcher
source common.sh

function build() {
    cd "$SOURCE_DIR"
    make $PLATFORM
}

function install() {
    cd "$SOURCE_DIR"
    mkdir -p "$INSTALL_DIR"
    case "$PLATFORM" in
        win) cp "portacle.exe" "$INSTALL_DIR/portacle";;
        *) cp "portacle" "$INSTALL_DIR/portacle" ;;
    esac
    cd "$SHARED_BIN_DIR"
    ln -rs "../$PROGRAM/portacle" "sbcl"
    ln -rs "../$PROGRAM/portacle" "git"
    ln -rs "../$PROGRAM/portacle" "emacs"
}

main
