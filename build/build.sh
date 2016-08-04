#!/bin/bash

TARGET=${1:-all}

function reset() {
    git clean -ffxd
    git reset --hard HEAD
}

function update() {
    git pull origin master
}

function global() {
    ./global.sh "$@"
}

function cl() {
    ./sbcl.sh "$@" \
        && ./asdf.sh "$@" \
        && ./quicklisp.sh "$@"
}

function editor() {
    ./emacs.sh "$@" \
        && ./emacsd.sh "$@"
}

function utils() {
    ./git.sh "$@" \
        && ./aspell.sh "$@" \
        && ./aspell-dicts.sh "$@" \
}

function package() {
    ./package.sh "$@"
}

function all() {
    global "$@" \
        && cl "$@" \
        && editor "$@" \
        && utils "$@" \
        && package "$@"
}

function fresh() {
    reset \
        && update \
        && all
}

function main() {
    $TARGET
}

main
