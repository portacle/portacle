#!/bin/bash

source common.sh

PACKAGE_DIR=$PORTACLE_DIR/package/
PACKAGE_VERS=$(git rev-parse --short HEAD)
PACKAGE_FILE=$PLATFORM-portacle-$PACKAGE_VERS

function win-translate-paths() {
    local result=()
    for file in "$@"; do
        result+=($(cygpath -w "$file"))
    done
    echo "${result[@]}"
}

function relativise-paths() {
    local result=()
    for file in "$@"; do
        result+=($(basename "$file"))
    done
    echo "${result[@]}"
}

function package-files(){
    local package="$1"
    local files=("${@:2}")
    
    case "$PLATFORM" in
        win) local winfile=$(win-translate-paths "$package")
             local winfiles=($(win-translate-paths "$files"))
             "/c/Program Files/7-zip/7z.exe" a -t7z "$winfile.exe" -m0=LZMA2 -mmt2 -sfx7z.sfx -aoa -r -snh -snl -ssw -y "${winfiles[@]}" \
                 || eexit "Could not create package."
             ;;
        *)   tar -cJf "$package.tar.xz" "${files[@]}" \
                 || eexit "Could not create package."
             ;;
    esac
}

function package-portacle-dir() {
    local package="$1"
    local dir="$2"
    mkdir -p $(dirname "$package")
    cd $(dirname "$dir")

    package-files "$package" $(basename "$dir")
}

function discover-files() {
    ls -rt -d -1 "$1/"{*,.*} | egrep -v "build|package|/\\.{1,2}$"
}

function prepare-for-packaging() {
    local source="$1"
    local target="$2"
    local files=($(discover-files "$source"))

    mkdir -p "$target"
    if system-has rsync; then
        rsync -avz "${files[@]}" "$target"
    else
        cp -rfuv "${files[@]}" "$target"
    fi
}

function package() {
    local target="$PACKAGE_DIR/portacle"
    prepare-for-packaging "$PORTACLE_DIR" "$target"
    package-portacle-dir "$PACKAGE_DIR/$PACKAGE_FILE" "$target"
}

function ensure-global-deps() {
    case "$PLATFORM" in
        lin) cp -fuv "$SHARED_DIR/lib/ld-linux.so" "/lib64/ld-linux-x86-64.so.2" ;;
    esac
}

function main() {
    ensure-global-deps
    package
}

main
