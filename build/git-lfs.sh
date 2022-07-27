#!/bin/bash

readonly TAG=v3.2.0
readonly REPOSITORY=https://github.com/git-lfs/git-lfs.git

###

readonly PROGRAM=git-lfs
source common.sh

function build(){
    cd "$SOURCE_DIR"
    make \
        || eexit "The build failed. Please check the output for error messages."
}

function install(){
    mkdir -p "$SHARED_BIN_DIR/"
    cp "$SOURCE_DIR/bin/git-lfs" "$SHARED_BIN_DIR/"
}

main
