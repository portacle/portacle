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
    status 1 "WARNING: Deleting all build files and resetting everything to as it is on git HEAD!"
    status 1 "         If this is not what you want, press Ctrl+C now!"
    sleep 1; status 2 "5"
    sleep 1; status 2 "4"
    sleep 1; status 2 "3"
    sleep 1; status 2 "2"
    sleep 1; status 2 "1"
    sleep 1;
    "$(which git)" clean -ffxd
    "$(which git)" reset --hard HEAD
    rm -rf "$PORTACLE_DIR/all"
    rm -rf "$PORTACLE_DIR/config"
    rm -rf "$PORTACLE_DIR/$PLATFORM"
    rm -rf "$PORTACLE_DIR/package/portacle"
}

function download() {
    "$(which git)" pull origin master
}

function global() {
    "$SOURCE_DIR/global.sh"
}

function config() {
    "$SOURCE_DIR/config.sh"
}

function launcher() {
    "$SOURCE_DIR/launcher.sh"
}

function busybox() {
    "$SOURCE_DIR/busybox.sh"
}

function sbcl() {
    "$SOURCE_DIR/sbcl.sh"
}

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

function git-lfs() {
    "$SOURCE_DIR/git-lfs.sh"
}

function hunspell() {
    "$SOURCE_DIR/hunspell.sh" \
        && "$SOURCE_DIR/dictionaries.sh"
}

function ag() {
    "$SOURCE_DIR/ag.sh"
}

function package() {
    "$SOURCE_DIR/package.sh"
}

function build() {
    global \
        && config \
        && launcher \
        && busybox \
        && sbcl \
        && asdf \
        && quicklisp \
        && emacs \
        && emacsd \
        && git \
        && git-lfs \
        && hunspell \
        && ag
}

function upgrade() {
    ## Download and then rerun self in case of changes.
    download \
        && "$SCRIPT_DIR/build.sh" build
}

function refresh() {
    ## Upgrade might cause changes, rerun self.
    clean \
        && upgrade \
        && "$SCRIPT_DIR/build.sh" package
}

main
