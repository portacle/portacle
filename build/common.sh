#!/bin/bash

function mreadlink() {
    python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1
}

case "$OSTYPE" in
    linux-gnu) OS="lin" ;;
    darwin*)   OS="mac" ;;
    cygwin)    OS="win" ;;
    msys)      OS="win" ;;
    win32)     OS="win" ;;
    freebsd)   OS="bsd" ;;
    *)         OS="any" ;;
esac

PLATFORM=${PLATFORM:-$OS}

case "$OS" in
    mac)  SCRIPT_DIR=$(mreadlink $(dirname $0)) ;;
    *)    SCRIPT_DIR=$(readlink -f $(dirname $0)) ;;
esac

SOURCE_DIR=$SCRIPT_DIR/$PROGRAM/
INSTALL_TARGET=$SCRIPT_DIR/../$PROGRAM/$PLATFORM/
TARGET=${1:-run_all}

case "$PLATFORM" in
    lin) MAXCPUS=$(cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1) ;;
    *)   MAXCPUS=1;;
esac

function nonlocal-ldd() {
    case "$PLATFORM" in
        win) ldd "$1" | grep "mingw" | awk -F\  '{print $3}' ;;
        mac) otool -L "$1" | grep -E "/opt/local/lib|/usr/local/lib" | awk -F\  '{print $1}' ;;
        *) ;;
    esac
}

function compute-dependencies() {
    local deps=( $(nonlocal-ldd $1) )
    local fulldeps=( "${deps[@]}" )
    for dep in "${deps[@]}"; do
        local newdeps=$(compdeps "$dep")
        fulldeps=( "${fulldeps[@]}" "${newdeps[@]}" )
    done
    echo "${fulldeps[@]}"
}

function ensure-installed() {
    local target=$1
    local files=( "${@:2}" )
    for file in "${files[@]}"; do
        local name=$(basename "$file")
        if [ ! -f "$target/$name" ]; then
            cp "$file" "$target/$name"
        fi
    done
}

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
