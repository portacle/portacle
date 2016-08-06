#!/bin/bash

readonly TAG=${PACKAGE_VERSION:-$(git describe --tags)}
readonly REPOSITORY=
readonly W7Z="/c/Program Files/7-zip/7z.exe"

##

readonly PROGRAM=package
source common.sh
readonly PACKAGE_FILE=${PACKAGE_FILE:-$PLATFORM-portacle-$TAG}

function win-translate-paths() {
    local result=()
    for file in "$@"; do
        result+=($(cygpath -w "$file"))
    done
    echo "${result[@]}"
}

function discover-files() {
    ls -rt -d -1 "$1/"{*,.*} | egrep -v "build|$PROGRAM|/\\.{1,2}\$"
}

function prepare() {
    mkdir -p "$INSTALL_TARGET"
}

function build() {
    local files=($(discover-files "$PORTACLE_DIR"))
    
    if system-has rsync; then
        rsync -avz "${files[@]}" "$INSTALL_TARGET"
    else
        case "$PLATFORM" in
            mac) cp -Rfv "${files[@]}" "$INSTALL_TARGET" ;;
            *)   cp -Rfuv "${files[@]}" "$INSTALL_TARGET" ;;
        esac
    fi

    ## Clean out known artefacts
    rm -rf "$INSTALL_TARGET/asdf/cache"
}

function install() {
    local package="$PORTACLE_DIR/$PROGRAM/$PACKAGE_FILE"
    local files=$(basename "$INSTALL_TARGET")
    
    cd $(dirname "$INSTALL_TARGET")
    case "$PLATFORM" in
        win) local winfile=$(win-translate-paths "$package")
             local winfiles=($(win-translate-paths "$files"))
             "$W7Z" a -t7z "$winfile.exe" -m0=LZMA2 -mmt2 -sfx7z.sfx -aoa -r -snh -snl -ssw -y "${winfiles[@]}" \
                 || eexit "Could not create package."
             ;;
        *)   tar -cJf "$package.tar.xz" "${files[@]}" \
                 || eexit "Could not create package."
             ;;
    esac
}

main
