#!/bin/bash

readonly TAG=v3.2.0
readonly REPOSITORY=https://github.com/git-lfs/git-lfs.git

###

readonly PROGRAM=git-lfs
source common.sh
INSTALL_DIR=$PORTACLE_DIR

function build(){
    cd "$SOURCE_DIR"
    make \
        || eexit "The build failed. Please check the output for error messages."
}

function install(){
    make install \
         || eexit "The install failed. Please check the output for error messages."
}
