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
    case "$PLATFORM" in
        mac) ./configure --prefix="$INSTALL_TARGET" --with-ns $CONFIGURE_OPTIONS ;;
        *)   ./configure --prefix="$INSTALL_TARGET" $CONFIGURE_OPTIONS ;;
    esac
}

function build() {
    cd "$SOURCE_DIR"
    make -j $MAXCPUS
}

function install() {
    cd "$SOURCE_DIR"
    make install datadir="$SHARE_TARGET"

    case "$PLATFORM" in
        win) cp "/mingw64/bin/libwinpthread-1.dll" "$INSTALL_TARGET/bin/" ;;
    esac
}

main
