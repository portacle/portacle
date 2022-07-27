#!/bin/bash

readonly TAG=gnupg-2.3.0
readonly REPOSITORY=https://github.com/gpg/gnupg
readonly CONFIGURE_OPTIONS=("")

###

readonly PROGRAM=gpg
source common.sh

function prepare() {
    cd "$SOURCE_DIR"
    ./autogen.sh
    ./configure --prefix="$INSTALL_DIR" "${CONFIGURE_OPTIONS[@]}" \
        || eexit "Configure failed. Maybe some dependencies are missing?"
}

function build() {
    cd "$SOURCE_DIR"
    local makeopts=""
    
    make -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function install() {
    cd "$SOURCE_DIR"
    make install \
        || eexit "The install failed. Please check the output for error messages."

    status 2 "Copying dependencies"
    ensure-dependencies $(find-binaries "$INSTALL_DIR/")

    case "$PLATFORM" in
        mac) mac-fixup-dependencies "$INSTALL_DIR/bin/gpg"
             mac-fixup-lib-dependencies
             ;;
    esac
}

main
