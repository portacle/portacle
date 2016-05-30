#!/bin/bash

readonly TAG=v2.9.0-rc0
#readonly REPOSITORY=https://github.com/git/git
readonly CONFIGURE_OPTIONS="--without-python --without-tcltk"
readonly MAKE_OPTIONS="NO_PERL=1"

###

readonly PROGRAM=git
source common.sh

if [ -z "$REPOSITORY" ]; then
    case "$PLATFORM" in
        win) readonly REPOSITORY=https://github.com/git-for-windows/git ;;
        *)   readonly REPOSITORY=https://github.com/git/git ;;
    esac
fi

function prepare() {
    cd "$SOURCE_DIR"
    make configure

    ## Ok, so, funny story. Configure fails if we don't tell it the socklen
    ## explicitly, but if we do specify that, it generates some new code that
    ## then clashes with the rest of the MinGW environment.
    ## In light of this, the current approach is to just let configure fail
    ## its test and build anyway.
    # case "$PLATFORM" in
    #     win) config_opts="$CONFIGURE_OPTIONS git_cv_socklen_t_equiv=int" ;;
    #     *)   config_opts="$CONFIGURE_OPTIONS" ;;
    # esac
    
    ./configure --prefix="$INSTALL_TARGET" $CONFIGURE_OPTIONS
}

function build() {
    cd "$SOURCE_DIR"
    make $MAKE_OPTIONS all -j $MAXCPUS
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

function win-ensure-dlls() {
    local exefiles=( $(ls "$INSTALL_TARGET/bin/" | grep "exe") )
    for exe in "${exefiles[@]}"; do
        ensure-installed "$INSTALL_TARGET/bin/" $(compute-dependencies "$INSTALL_TARGET/bin/$exe")
    done
    ## Libcurl won't appear in that because it's dynamically linked. 
    ensure-installed "$INSTALL_TARGET/bin/" "/mingw64/bin/libcurl-4.dll" $(compute-dependencies "/mingw64/bin/libcurl-4.dll")   
}

function install() {
    cd "$SOURCE_DIR"
    make prefix="$INSTALL_TARGET" $MAKE_OPTIONS install

    case "$PLATFORM" in
        win) ## It creates hardlinks which are hard to ship. We'll convert them into symlinks.
            win-convert-hardlinks
            ## Ok, now we need to copy the core utilities. This is necessary because some parts
            ## of git are mere bash scripts, which then in turn need to run the core util programs
            ## which means we have to ship an almost complete MSYS. How fantastic.
            win-copy-coreutils
            ## Ensure DLL dependencies for all.
            win-ensure-dlls
            ;;
        mac) ## Copy dylibs and such.
            ensure-installed "$INSTALL_TARGET/bin/" $(compute-dependencies "$INSTALL_TARGET/bin/git")
            ;;
    esac
}

main
