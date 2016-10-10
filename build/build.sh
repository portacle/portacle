#!/bin/bash

readonly TAG=$(git describe --tags)
readonly REPOSITORY=https://github.com/Shinmera/portacle

##

readonly PROGRAM=portacle
DEFAULT_TARGETS=(download upgrade package)
source common.sh

SOURCE_DIR="$SCRIPT_DIR"
INSTALL_TARGET="$PORTACLE_DIR"

function clean() {
    "$(which git)" clean -ffxd
    "$(which git)" reset --hard HEAD
}

function download() {
    "$(which git)" pull origin master
}

function global() {
    "$SOURCE_DIR/global.sh"
}

function sbcl() {
    "$SOURCE_DIR/sbcl.sh"

function asdf() {
    "$SOURCE_DIR/asdf.sh"
}

function quicklisp() {
    "$SOURCE_DIR/quicklisp.sh"
}

function emacs() {
    "$SOURCE_DIR/emacs.sh"
}

function emacsd() {
    "$SOURCE_DIR/emacsd.sh"
}

function git() {
    "$SOURCE_DIR/git.sh"
}

function package() {
    "$SOURCE_DIR/package.sh"
}

function upgrade() {
    download \
        && global \
        && sbcl \
        && asdf \
        && quicklisp \
        && emacs \
        && emacsd \
        && git
}

function refresh() {
    clean \
        && upgrade \
        && package
}

main
