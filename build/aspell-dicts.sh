#!/bin/bash

readonly TAG=1.0.0
readonly REPOSITORY=https://github.com/shinmera/aspell-dicts
readonly DICTS=( "en" )

###

readonly PROGRAM=aspell-dicts
source common.sh
INSTALL_TARGET=$SCRIPT_DIR/../aspell/share/

case "$PLATFORM" in
    win) ASPELL=$SCRIPT_DIR/../aspell/$PLATFORM/aspell.bat ;;
    *)   ASPELL=$SCRIPT_DIR/../aspell/$PLATFORM/aspell.sh ;;
esac


function prepare() {
    cd "$SOURCE_DIR"
    for dict in "${DICTS[@]}"; do
        cd "$dict"
        ./configure --vars ASPELL="$ASPELL"
    done
}

function build() {
    cd "$SOURCE_DIR"
    for dict in "${DICTS[@]}"; do
        cd "$dict"
        make
    done
}

function install() {
    cd "$SOURCE_DIR"
    for dict in "${DICTS[@]}"; do
        cd "$dict"
        make install
    done
}

main
