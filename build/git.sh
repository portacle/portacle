#!/bin/bash

readonly TAG=v2.9.0-rc0
readonly REPOSITORY=https://github.com/git/git
readonly CONFIGURE_OPTIONS="--without-python --without-tcltk"
readonly MAKE_OPTIONS="NO_PERL=1"

###

readonly PROGRAM=git
source common.sh

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

function install() {
    cd "$SOURCE_DIR"
    make prefix="$INSTALL_TARGET" $MAKE_OPTIONS install

    ## Copy MinGW dlls
    case "$PLATFORM" in
        win) ensure-installed "$INSTALL_TARGET/bin/" $(compute-dependencies "$INSTALL_TARGET/bin/git") ;;
        mac) ensure-installed "$INSTALL_TARGET/bin/" $(compute-dependencies "$INSTALL_TARGET/bin/git") ;;
    esac
}

main
