#!/bin/bash

###

readonly PROGRAM=global
source common.sh

function download() {
    echo "Skipping download"
}

function install() {
    case "$PLATFORM" in
        lin) cp -fuv "/lib64/ld-linux-x86-64.so.2" "$SHARED_DIR/lib/ld-linux.so" ;;
    esac
}

main
