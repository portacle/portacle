#!/bin/bash

readonly MAKE_OPTIONS="USE_LIBPCRE=1 NO_PERL=1 NO_SVN_TESTS=1 NO_PYTHON=1 NO_TCLTK=1 NO_INSTALL_HARDLINKS=1"

###

readonly PROGRAM=git
source common.sh

case "$PLATFORM" in
    win) readonly REPOSITORY=https://github.com/git-for-windows/git
         readonly TAG=v2.9.3.windows.1;;
    *)   readonly REPOSITORY=https://github.com/git/git
         readonly TAG=v2.9.3;;
esac

function build() {
    cd "$SOURCE_DIR"
    make DESTDIR="$PORTACLE_DIR" prefix="/$PLATFORM/git/" $MAKE_OPTIONS all -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function win-copy-coreutils() {
    ensure-installed "$1" $(win-exes-for-package bash) \
                     $(win-exes-for-package coreutils) \
                     $(win-exes-for-package openssh) \
                     $(win-exes-for-package grep) \
                     $(win-exes-for-package less) \
                     $(win-exes-for-package sed) \
                     $(win-exes-for-package msys2-runtime) \
                     $(win-exes-for-package ncurses)
    ensure-dependencies $(find-binaries "$1")
}

function ensure-git-platform() {
    mkdir -p "$INSTALL_DIR/share/ssl"
    cp "$PORTACLE_DIR/all/ssl/ca-bundle.crt" "$INSTALL_DIR/share/ssl/ca-bundle.crt"
    case "$PLATFORM" in
        win) win-copy-coreutils "$SHARED_BIN_DIR/"
             ensure-installed "$SHARED_LIB_DIR/" "/mingw64/bin/libcurl-4.dll"
             ensure-installed "$SHARED_DIR/share/" "/usr/lib/terminfo"
             ensure-dependencies "/mingw64/bin/libcurl-4.dll"
             mkdir -p "$PORTACLE_DIR/tmp"
             ;;
        lin) ensure-installed "$SHARED_LIB_DIR/" "/usr/lib/libcurl.so"
             ensure-dependencies "/usr/lib/libcurl.so"
             ensure-dependencies "/usr/bin/ssh"
             ;;
    esac
}

function install() {
    cd "$SOURCE_DIR"
    make DESTDIR="$PORTACLE_DIR" prefix="/$PLATFORM/git/" $MAKE_OPTIONS install \
        || eexit "The install failed. Please check the output for error messages."

    status 2 "Copying platform"
    ensure-git-platform

    status 2 "Copying dependencies"
    ensure-dependencies $(find-binaries "$INSTALL_DIR/")
}

main
