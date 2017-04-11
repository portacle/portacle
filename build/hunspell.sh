#!/bin/bash

readonly TAG=v1.6.1
readonly REPOSITORY=https://github.com/hunspell/hunspell
readonly CONFIGURE_OPTIONS=()

###

readonly PROGRAM=hunspell
source common.sh

function prepare() {
    cd "$SOURCE_DIR"
    autoreconf -vfi
    ./configure --prefix="$INSTALL_DIR" "${CONFIGURE_OPTIONS[@]}"
}

function build() {
    cd "$SOURCE_DIR"
    make -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function install() {
    cd "$SOURCE_DIR"
    make install \
        || eexit "The install failed. Please check the output for error messages."

    status 2 "Copying dependencies"
    ensure-dependencies $(find-binaries "$INSTALL_DIR/")
}

main
