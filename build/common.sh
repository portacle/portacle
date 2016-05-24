#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    OS="lin"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    OS="mac"
elif [[ "$OSTYPE" == "cygwin" ]]; then
    OS="win"
elif [[ "$OSTYPE" == "msys" ]]; then
    OS="win"
elif [[ "$OSTYPE" == "win32" ]]; then
    OS="win"
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    OS="bsd"
else
    OS="any"
fi

PLATFORM=${PLATFORM:-$OS}
SCRIPT_DIR=$(readlink -m $(dirname $0))
SOURCE_DIR=$SCRIPT_DIR/$PROGRAM/
INSTALL_TARGET=$SCRIPT_DIR/../$PROGRAM/$PLATFORM/
TARGET=${1:-run_all}
MAXCPUS=$(cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1)

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
