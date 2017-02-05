#!/bin/bash

readonly TAG=1_25_1
readonly REPOSITORY=git://busybox.net/busybox.git

###

readonly PROGRAM=busybox
source common.sh
INSTALL_DIR="$SHARED_DIR"

function prepare() {
    cd "$SOURCE_DIR"
    make defconfig
    echo -e "CONFIG_INSTALL_NO_USR=y\n$(cat .config)" > .config
    echo -e "CONFIG_STATIC=y\n$(cat .config)" > .config
}

function build() {
    cd "$SOURCE_DIR"
    make -j $MAXCPUS \
         || eexit "The build failed. Please check the output for error messages."
}

function install() {
    cd "$SOURCE_DIR"
    make CONFIG_PREFIX="$INSTALL_DIR" install\
        || eexit "The install failed. Please check the output for error messages."
}

main
