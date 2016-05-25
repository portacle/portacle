#!/bin/bash

case "$OSTYPE" in
    linux-gnu) OS="lin" ;;
    darwin)    OS="mac" ;;
    cygwin)    OS="win" ;;
    msys)      OS="win" ;;
    win32)     OS="win" ;;
    freebsd)   OS="bsd" ;;
    *)         OS="any" ;;
esac

PLATFORM=${PLATFORM:-$OS}
SCRIPT_DIR=$(readlink -m $(dirname $0))
SOURCE_DIR=$SCRIPT_DIR/$PROGRAM/
INSTALL_TARGET=$SCRIPT_DIR/../$PROGRAM/$PLATFORM/
TARGET=${1:-run_all}

case "$PLATFORM" in
    lin) MAXCPUS=$(cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1) ;;
    *)   MAXCPUS=1;;
esac

function eexit() {
    echo "! Error: $@"
    exit 1
}

function info() {
    echo "  Build info:
Platform:           ${PLATFORM}
Downloading from:   ${REPOSITORY}
Using tag:          ${TAG}
Building in:        ${SOURCE_DIR}
Installing into:    ${INSTALL_TARGET}

"
}

function download() {
    mkdir -p "$SOURCE_DIR" &> /dev/null
    if [ -d "$SOURCE_DIR/.git" ]; then
        cd "$SOURCE_DIR"
        git reset --hard HEAD
        git clean -fdx
        git checkout master
        git pull \
            || eexit "Failed to download source."
    else
        git clone "$REPOSITORY" "$SOURCE_DIR" \
            || eexit "Failed to download source."
        cd "$SOURCE_DIR"
    fi
    if [ -n "$TAG" ]; then
       git checkout tags/$TAG \
           || eexit "Failed to checkout desired tag."
    fi
}

function prepare (){
    echo "Skipping prepare"
}

function build (){
    echo "Skipping build"
}

function install (){
    echo "Skipping install"
}

function clean() {
    rm -rf "$SOURCE_DIR"
}

function clean_installed() {
    cd "$INSTALL_TARGET"
    rm -R ./*/
}

function run_all() {
    info
    download
    prepare
    build
    install
}

function main() {
    $TARGET
}
