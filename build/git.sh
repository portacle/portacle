#!/bin/bash

readonly TAG=v2.9.0-rc0
readonly REPOSITORY=https://github.com/git/git
readonly CONFIGURE_OPTIONS="--without-python --without-tcltk"
readonly MAKE_OPTIONS="NO_PERL=1"

###

readonly PROGRAM=git
source common.sh

function prepare() {
    cd "$SOURCE_DIR"
    make configure
    ./configure --prefix="$INSTALL_TARGET" $CONFIGURE_OPTIONS
}

function build() {
    cd "$SOURCE_DIR"
    make all -j $MAXCPUS $MAKE_OPTIONS
}

function install() {
    cd "$SOURCE_DIR"
    make install $MAKE_OPTIONS
}

main
