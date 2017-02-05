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
        lin) cp "portacle" "ld-wrap.so" "$INSTALL_DIR/" ;;
        *)   cp "portacle" "$INSTALL_DIR/" ;;
    esac
    cd "$SHARED_BIN_DIR"
    ln -fs "../$PROGRAM/portacle" "sbcl"
    ln -fs "../$PROGRAM/portacle" "git"
    ln -fs "../$PROGRAM/portacle" "emacs"
}

main
