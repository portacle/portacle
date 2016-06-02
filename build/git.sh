#!/bin/bash

readonly TAG=v2.8.3
#readonly REPOSITORY=https://github.com/git/git
readonly MAKE_OPTIONS="USE_LIBPCRE=1 NO_PERL=1 NO_GETTEXT=1 NO_SVN_TESTS=1 NO_PYTHON=1 NO_TCLTK=1 NO_INSTALL_HARDLINKS=1"

###

readonly PROGRAM=git
source common.sh

if [ -z "$REPOSITORY" ]; then
    case "$PLATFORM" in
        win) readonly REPOSITORY=https://github.com/git-for-windows/git ;;
        *)   readonly REPOSITORY=https://github.com/git/git ;;
    esac
fi

function build() {
    cd "$SOURCE_DIR"
    make $MAKE_OPTIONS all -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function win-convert-hardlinks() {
    local links=( $(find "$INSTALL_TARGET/" -samefile "$INSTALL_TARGET/bin/git.exe") )
    for link in "${links[@]}"; do
        if [ "$link" != "$INSTALL_TARGET/bin/git.exe" ]; then
            local winpath=$(cygpath -w "$link")
            eecho "Converting hard link $link"
            rm "$link"
            cmd /c "mklink \"$winpath\" \"..\\..\\bin\\git.exe\""
        fi
    done
}

function win-copy-coreutils() {
    local coreutils=( $(pacman -Ql coreutils | grep "exe" | awk '{print $2}') )
    ensure-installed "$INSTALL_TARGET/bin/" "/usr/bin/bash.exe" "/usr/bin/sh.exe" "${coreutils[@]}"
}

function install() {
    cd "$SOURCE_DIR"
    make prefix="$INSTALL_TARGET" $MAKE_OPTIONS install \
        || eexit "The install failed. Please check the output for error messages."

    case "$PLATFORM" in
        win) win-convert-hardlinks
             win-copy-coreutils
             ensure-installed "$SHARED_DIR" "/mingw64/bin/libcurl-4.dll"
             ensure-dependencies "/mingw64/bin/libcurl-4.dll"
             ;;
        lin) ensure-installed "$SHARED_DIR" "/usr/lib/libcurl.so"
             ensure-dependencies "/usr/lib/libcurl.so"
             ;;
    esac
    
    ensure-dependencies $(find-binaries "$INSTALL_TARGET/")
}

main
