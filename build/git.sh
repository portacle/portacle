#!/bin/bash

#readonly TAG=v2.8.3
#readonly REPOSITORY=https://github.com/git/git
readonly MAKE_OPTIONS="USE_LIBPCRE=1 NO_PERL=1 NO_SVN_TESTS=1 NO_PYTHON=1 NO_TCLTK=1 NO_INSTALL_HARDLINKS=1"

###

readonly PROGRAM=git
source common.sh

if [ -z "$REPOSITORY" ]; then
    case "$PLATFORM" in
        win) readonly REPOSITORY=https://github.com/git-for-windows/git
             readonly TAG=v2.8.3.windows.1;;
        *)   readonly REPOSITORY=https://github.com/git/git
             readonly TAG=v2.8.3;;
    esac
fi

function build() {
    cd "$SOURCE_DIR"
    make prefix="$INSTALL_TARGET" $MAKE_OPTIONS all -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function win-copy-coreutils() {
    local coreutils=( $(pacman -Ql coreutils | grep "exe" | awk '{print $2}') )
    local openssh=( $(pacman -Ql openssh | grep "exe" | awk '{print $2}') )
    ensure-installed "$1" "/usr/bin/bash.exe" "/usr/bin/sh.exe" "${coreutils[@]}" "${openssh[@]}"
    ensure-dependencies $(find-binaries "$1")
}

function install() {
    cd "$SOURCE_DIR"
    make prefix="$INSTALL_TARGET" $MAKE_OPTIONS install \
        || eexit "The install failed. Please check the output for error messages."

    case "$PLATFORM" in
        win) win-copy-coreutils "$SHARED_DIR/bin/"
             ensure-installed "$SHARED_DIR/ssl/" "/usr/ssl/certs/ca-bundle.crt"
             ensure-installed "$SHARED_DIR/lib/" "/mingw64/bin/libcurl-4.dll"
             ensure-dependencies "/mingw64/bin/libcurl-4.dll"
             ;;
        lin) ensure-installed "$SHARED_DIR/lib/" "/usr/lib/libcurl.so"
             ensure-dependencies "/usr/lib/libcurl.so"
             ;;
    esac
    
    ensure-dependencies $(find-binaries "$INSTALL_TARGET/")
}

main
