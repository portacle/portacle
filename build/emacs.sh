#!/bin/bash

readonly TAG=emacs-25.0.93
readonly REPOSITORY=git://git.savannah.gnu.org/emacs.git
readonly CONFIGURE_OPTIONS=" --without-jpeg --without-tiff --without-gif --without-png --without-rsvg --without-imagemagick --without-sound"

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
    make -j $MAXCPUS
}

function install() {
    cd "$SOURCE_DIR"
    make install datadir="$SHARE_TARGET"
    
    if [[ "$PLATFORM" == "win" ]]; then
        cp /mingw64/bin/libwinpthread-1.dll "$INSTALL_TARGET/bin/"
    fi
}

main
