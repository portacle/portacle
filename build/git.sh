#!/bin/bash

#readonly TAG=v2.8.3
#readonly REPOSITORY=https://github.com/git/git
readonly MAKE_OPTIONS="USE_LIBPCRE=1 NO_PERL=1 NO_SVN_TESTS=1 NO_PYTHON=1 NO_TCLTK=1 NO_INSTALL_HARDLINKS=1"

###

readonly PROGRAM=git
source common.sh

case "$PLATFORM" in
    win) readonly REPOSITORY=https://github.com/git-for-windows/git
         readonly TAG=v2.8.3.windows.1;;
    *)   readonly REPOSITORY=https://github.com/git/git
         readonly TAG=v2.8.3;;
esac

function build() {
    cd "$SOURCE_DIR"
    make DESTDIR="$PORTACLE_DIR" prefix="/git/$PLATFORM/" $MAKE_OPTIONS all -j $MAXCPUS \
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

function install() {
    cd "$SOURCE_DIR"
    make DESTDIR="$PORTACLE_DIR" prefix="/git/$PLATFORM/" $MAKE_OPTIONS install \
        || eexit "The install failed. Please check the output for error messages."

    case "$PLATFORM" in
        win) win-copy-coreutils "$SHARED_DIR/bin/"
             ensure-installed "$SHARED_DIR/ssl/" "/usr/ssl/certs/ca-bundle.crt"
             ensure-installed "$SHARED_DIR/lib/" "/mingw64/bin/libcurl-4.dll"
             ensure-installed "$SHARED_DIR/share/" "/usr/lib/terminfo"
             ensure-dependencies "/mingw64/bin/libcurl-4.dll"
             mkdir -p "$PORTACLE_DIR/tmp"
             ## Fix symlinks
             cd "$PORTACLE_DIR"
             git config core.symlinks true
             git checkout "git/*/etc/gitconfig"
             git update-index --assume-unchanged "git/*/etc/gitconfig"
             ;;
        lin) ensure-installed "$SHARED_DIR/lib/" "/usr/lib/libcurl.so"
             ensure-dependencies "/usr/lib/libcurl.so"
             ;;
    esac
    
    ensure-dependencies $(find-binaries "$INSTALL_TARGET/")
}

main
