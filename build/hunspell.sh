#!/bin/bash

readonly TAG=v1.4.1
readonly REPOSITORY=https://github.com/hunspell/hunspell

###

readonly PROGRAM=hunspell
source common.sh

function prepare() {
    cd "$SOURCE_DIR"
    autoreconf && automake --add-missing \
            || eexit "Failed to generate configure."
    ./configure --prefix="/" --disable-shared --disable-static \
        || eexit "Configure failed. Maybe some dependencies are missing?"
}

function build() {
    cd "$SOURCE_DIR"
    make -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function install() {
    cd "$SOURCE_DIR"
    make DESTDIR="$INSTALL_TARGET" install \
        || eexit "The install failed. Please check the output for error messages."    
    
    ensure-dependencies $(find-binaries "$INSTALL_TARGET/")
}

main
