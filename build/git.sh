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

    ## Configure fails without this hint on windows.
    case "$PLATFORM" in
        win) config_opts="$CONFIGURE_OPTIONS git_cv_socklen_t_equiv=int" ;;
        *)   config_opts="$CONFIGURE_OPTIONS" ;;
    esac
    
    ./configure --prefix="$INSTALL_TARGET" $config_opts
}

function build() {
    cd "$SOURCE_DIR"
    make $MAKE_OPTIONS all -j $MAXCPUS
}

function install() {
    cd "$SOURCE_DIR"
    make prefix="$INSTALL_TARGET" $MAKE_OPTIONS install
}

main
