#!/bin/bash

readonly TAG=1_25_1
readonly REPOSITORY=git://busybox.net/busybox.git

###

readonly PROGRAM=busybox
source common.sh
INSTALL_DIR="$SHARED_DIR"

function prepare() {
    cd "$SOURCE_DIR"

    case "$PLATFORM" in
        win) status 2 "Skipping prepare on Windows." ;;
        mac) status 2 "Skipping prepare on OS X." ;;
        lin) make defconfig
             echo -e "CONFIG_INSTALL_NO_USR=y\n$(cat .config)" > .config
             echo -e "CONFIG_STATIC=y\n$(cat .config)" > .config
             ;;
    esac
}

function build() {
    cd "$SOURCE_DIR"

    case "$PLATFORM" in
        win) status 2 "Skipping build on Windows." ;;
        mac) status 2 "Skipping build on OS X." ;;
        lin) make -j $MAXCPUS \
                   || eexit "The build failed. Please check the output for error messages." ;;
    esac
}

function install() {
    cd "$SOURCE_DIR"

    case "$PLATFORM" in
        win) status 2 "Installing coreutils from MSYS instead."
             ensure-installed "$SHARED_BIN_DIR/" \
                              $(win-exes-for-package bash) \
                              $(win-exes-for-package coreutils) \
                              $(win-exes-for-package grep) \
                              $(win-exes-for-package less) \
                              $(win-exes-for-package sed) \
                              $(win-exes-for-package msys2-runtime) \
                              $(win-exes-for-package ncurses)
             ensure-dependencies $(find-binaries "$SHARED_BIN_DIR/") ;;
        mac) status 2 "Skipping install, the OS provides the tools." ;;
        lin) make CONFIG_PREFIX="$INSTALL_DIR" install\
                   || eexit "The install failed. Please check the output for error messages." ;;
    esac
}

main
