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
    cd "$SOURCE_DIR/build"
    mkdir -p "$INSTALL_DIR"
    case "$PLATFORM" in
        win) cp "portacle.exe" "fontreg.exe" "$INSTALL_DIR/" ;;
        lin) cp "portacle" "fontreg" "ld-wrap.so" "$INSTALL_DIR/" ;;
        mac) cp "portacle" "fontreg" "$INSTALL_DIR/" ;;
    esac

    local postfix=""
    case "$PLATFORM" in
        win) postfix=".exe" ;;
    esac
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/sbcl${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/git${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/emacs${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/portacle${postfix}"
}

main
