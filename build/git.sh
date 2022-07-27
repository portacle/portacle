#!/bin/bash

MAKE_OPTIONS="USE_LIBPCRE=1 NO_PERL=1 NO_SVN_TESTS=1 NO_PYTHON=1 NO_TCLTK=1 NO_INSTALL_HARDLINKS=1"

###

readonly PROGRAM=git
source common.sh

case "$PLATFORM" in
    win) readonly REPOSITORY=https://github.com/git-for-windows/git
         readonly TAG=v2.37.1.windows.1;;
    *)   readonly REPOSITORY=https://github.com/git/git
         readonly TAG=v2.37.1;;
esac

function build() {
    cd "$SOURCE_DIR"

    # Windows requires gettext (vsnprintf broken error otherwise)
    # Mac OS X doesn't build with gettext
    # Linux can do without
    case "$PLATFORM" in
        win) ;;
        mac) MAKE_OPTIONS="NO_GETTEXT=1 CFLAGS=-I/usr/local/opt/openssl/include LDFLAGS=-L/usr/local/opt/openssl/lib $MAKE_OPTIONS" ;;
        lin) MAKE_OPTIONS="NO_GETTEXT=1 $MAKE_OPTIONS" ;;
    esac
    
    make DESTDIR="$PORTACLE_DIR" prefix="/$PLATFORM/git/" $MAKE_OPTIONS all -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function ensure-git-platform() {
    mkdir -p "$INSTALL_DIR/share/ssl"
    cp "$PORTACLE_DIR/all/ssl/ca-bundle.crt" "$INSTALL_DIR/share/ssl/ca-bundle.crt"
    case "$PLATFORM" in
        win) ensure-installed "$SHARED_BIN_DIR/" $(win-exes-for-package openssh)
             ensure-dependencies $(win-exes-for-package openssh)
             ensure-installed "$SHARED_LIB_DIR/" "/mingw64/bin/libcurl-4.dll"
             ensure-dependencies "/mingw64/bin/libcurl-4.dll"
             ensure-installed "$SHARED_DIR/share/" "/usr/lib/terminfo"
             mkdir -p "$PORTACLE_DIR/tmp"
             ;;
        lin) ensure-shared-libraries "$SHARED_LIB_DIR/" "libcurl"
             ensure-dependencies $SHARED_LIB_DIR/libcurl.so*
             ensure-dependencies "/usr/bin/ssh"
             ;;
        mac) mac-fixup-lib-dependencies
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
