#!/bin/bash

readonly TAG=
readonly REPOSITORY=https://github.com/portacle/config

###

readonly PROGRAM=config
source common.sh
INSTALL_DIR=$PORTACLE_DIR/$PROGRAM

function install() {
    cd "$SOURCE_DIR"
    mkdir -p "$INSTALL_DIR" \
        || eexit "Failed to create $INSTALL_DIR"
    cp -Rf "$SOURCE_DIR/." "$INSTALL_DIR" \
        || eexit "Failed to copy config files."
}

main
