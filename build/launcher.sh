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
        win) cp "portacle.exe" "$INSTALL_DIR/";;
        lin) cp "portacle" "ld-wrap.so" "$INSTALL_DIR/" ;;
        *)   cp "portacle" "$INSTALL_DIR/" ;;
    esac

    case "$PLATFORM" in
        win) ln -f "$INSTALL_DIR/portacle.exe" "$SHARED_BIN_DIR/sbcl.exe"
             ln -f "$INSTALL_DIR/portacle.exe" "$SHARED_BIN_DIR/git.exe"
             ln -f "$INSTALL_DIR/portacle.exe" "$SHARED_BIN_DIR/emacs.exe"
             ln -f "$INSTALL_DIR/portacle.exe" "$PORTACLE_DIR/portacle.exe" ;;
        *)   ln -f "$INSTALL_DIR/portacle" "$SHARED_BIN_DIR/sbcl"
             ln -f "$INSTALL_DIR/portacle" "$SHARED_BIN_DIR/git"
             ln -f "$INSTALL_DIR/portacle" "$SHARED_BIN_DIR/emacs" ;;
    esac
}

main
