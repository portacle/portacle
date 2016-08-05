#!/bin/bash

readonly TAG=$(git describe --tags)
readonly REPOSITORY=https://github.com/Shinmera/portacle

##

readonly PROGRAM=portacle
TARGETS=( ${@:-download upgrade package} )
source common.sh

SOURCE_DIR="$SCRIPT_DIR"
INSTALL_TARGET="$PORTACLE_DIR"

function clean() {
    git clean -fxd
    git reset --hard HEAD
}

function download() {
    git pull origin master
}

function global() {
    "$SOURCE_DIR/global.sh" "$@"
}

function sbcl() {
    "$SOURCE_DIR//sbcl.sh" "$@"
}

function asdf() {
    "$SOURCE_DIR/asdf.sh" "$@"
}

function quicklisp() {
    "$SOURCE_DIR/quicklisp.sh" "$@"
}

function emacs() {
    "$SOURCE_DIR/emacs.sh" "$@"
}

function emacsd() {
    "$SOURCE_DIR/emacsd.sh" "$@"
}

function git() {
    "$SOURCE_DIR/git.sh" "$@"
}

function package() {
    "$SOURCE_DIR/package.sh" "$@"
}

function upgrade() {
    global "$@" \
        && sbcl "$@" \
        && asdf "$@" \
        && quicklisp "$@" \
        && emacs "$@" \
        && emacsd "$@" \
        && git "$@"
}

function refresh() {
    clean "$@" \
        && update "$@" \
        && all "$@"
}

main
