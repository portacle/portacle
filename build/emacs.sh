#!/bin/bash

readonly TAG=emacs-25.0.93
readonly REPOSITORY=git://git.savannah.gnu.org/emacs.git
readonly CONFIGURE_OPTIONS="--without-all"

###

readonly PROGRAM=emacs
source common.sh
readonly SHARE_TARGET=$SCRIPT_DIR/../$PROGRAM/share/

function prepare() {
    cd "$SOURCE_DIR"
    ./autogen.sh
    ./configure --prefix="$INSTALL_TARGET" $CONFIGURE_OPTIONS
}

function build() {
    cd "$SOURCE_DIR"
    make
}

function install() {
    cd "$SOURCE_DIR"
    make install datadir="$SHARE_TARGET"
}

main
