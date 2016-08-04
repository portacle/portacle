#!/bin/bash

TARGETS=( ${@:-all} )

function info() {
    local VERSION=$(git describe --tags)
    echo "  Portacle Build
Version: $VERSION
Targets: ${TARGETS[@]}"
}

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
        && ./aspell-dicts.sh "$@"
}

function package() {
    ./package.sh "$@"
}

function upgrade() {
    global "$@" \
        && cl "$@" \
        && editor "$@" \
        && utils "$@"
}

function all() {
    upgrade "$@" \
        && package "$@"
}

function fresh() {
    reset "$@" \
        && update "$@" \
        && all "$@"
}

function main() {
    for TARGET in "${TARGETS[@]}"; do
        $TARGET || exit 1
    done
}

main
