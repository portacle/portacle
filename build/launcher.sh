#!/bin/bash

readonly TAG=
readonly REPOSITORY=https://github.com/portacle/launcher.git

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
    mkdir -p "$SHARED_BIN_DIR"
    mkdir -p "$SHARED_LIB_DIR"
    
    case "$PLATFORM" in
        win) cp "portacle.exe" "credentials" "$INSTALL_DIR/"
             ensure-dependencies "credentials" ;;
        lin) cp "portacle" "credentials" "ld-wrap.so" "$INSTALL_DIR/"
             cp "libnss_*.so.2" "$SHARED_LIB_DIR/"
             ensure-dependencies "credentials" ;;
        mac) cp "portacle" "credentials" "$INSTALL_DIR/"
             ensure-dependencies "credentials"
             mac-fixup-dependencies "$INSTALL_DIR/portacle"
             mac-fixup-dependencies "$INSTALL_DIR/credentials"
             mac-fixup-lib-dependencies ;;
    esac

    local postfix=""
    case "$PLATFORM" in
        win) postfix=".exe" ;;
    esac

    ln -f "$INSTALL_DIR/credentials${postfix}" "$SHARED_BIN_DIR/git-credential-portacle${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/sbcl${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/git${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/emacs${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/emacsclient${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/hunspell${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/ag${postfix}"
    ln -f "$INSTALL_DIR/portacle${postfix}" "$SHARED_BIN_DIR/portacle${postfix}"
}

main
