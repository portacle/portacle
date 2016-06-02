#!/bin/bash

## Determine the OS type
case "$OSTYPE" in
    linux-gnu) OS="lin" ;;
    darwin*)   OS="mac" ;;
    cygwin)    OS="win" ;;
    msys)      OS="win" ;;
    win32)     OS="win" ;;
    freebsd)   OS="bsd" ;;
    *)         OS="any" ;;
esac

## Use autodetect if unspecified
PLATFORM=${PLATFORM:-$OS}

## OS X' readlink does not support -f, substitute our own
function mreadlink() {
    case "$PLATFORM" in 
        mac) python -c 'import os, sys; print os.path.realpath(sys.argv[1])' $1 ;;
        *)   readlink -f $1 ;;
    esac
}

## Autodetect other variables
SCRIPT_DIR=${SCRIPT_DIR:-$(mreadlink $(dirname $0))}
SOURCE_DIR=${SOURCE_DIR:-$SCRIPT_DIR/$PROGRAM/}
PORTACLE_DIR=${PORTACLE_DIR:-$(mreadlink $SCRIPT_DIR/../)}
SHARED_DIR=${SHARED_DIR:-$PORTACLE_DIR/shared}
INSTALL_TARGET=${INSTALL_TARGET:-$PORTACLE_DIR/$PROGRAM/$PLATFORM/}
TARGET=${1:-run_all}

## Determine mac cpu count
case "$PLATFORM" in
    lin) MAXCPUS=${MAXCPUS:-$(cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1)} ;;
    *)   MAXCPUS=${MAXCPUS:-1} ;;
esac

## Find all the dynamic libraries not provided by the native OS
function nonlocal-ldd() {
    case "$PLATFORM" in
        win) ldd "$1" | awk '{print $3}' | grep -E '^/[^/]{2,}/' ;;
        mac) otool -L "$1" | grep -E "/opt/local/lib|/usr/local/lib" | awk -F\  '{print $1}' ;;
        *) ;;
    esac
}

## Compute the full dependency list for the given input file
function compute-dependencies() {
    local deps=( $(nonlocal-ldd $1) )
    local fulldeps=( "${deps[@]}" )
    for dep in "${deps[@]}"; do
        local newdeps=$(compute-dependencies "$dep")
        fulldeps=( "${fulldeps[@]}" "${newdeps[@]}" )
    done
    echo "${fulldeps[@]}"
}

## Ensure the given array of files is copied to the target directory
function ensure-installed() {
    local target=$1
    local files=( "${@:2}" )
    for file in "${files[@]}"; do
        local name=$(basename "$file")
        if [ ! -f "$target/$name" ]; then
            eecho "Copying $file"
            cp "$file" "$target/$name"
        fi
    done
}

## This does not need explanation
function eecho() {
    >2& echo $@
}

function eexit() {
    echo "! Error: $@"
    exit 1
}

## Following here are standard implementations for the various stages
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
