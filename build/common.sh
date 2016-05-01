#!/bin/bash

PLATFORM=${PLATFORM:-lin}
readonly SCRIPT_DIR=$(readlink -m $(dirname $0))
readonly SOURCE_DIR=$SCRIPT_DIR/$PROGRAM/
readonly INSTALL_TARGET=$SCRIPT_DIR/../$PROGRAM/$PLATFORM/
readonly TARGET=${1:-run_all}

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
    git checkout tags/${TAG} \
        || eexit "Failed to checkout desired tag."
}

function clean() {
    rm -rf "$SOURCE_DIR"
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
