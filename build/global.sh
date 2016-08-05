#!/bin/bash

readonly TAG=
readonly REPOSITORY=

###

readonly PROGRAM=global
source common.sh

function install() {
    mkdir -p "$SHARED_DIR/lib"
    case "$PLATFORM" in
        lin) cp -fuv "/lib64/ld-linux-x86-64.so.2" "$SHARED_DIR/lib/ld-linux.so" ;;
    esac
}

main
