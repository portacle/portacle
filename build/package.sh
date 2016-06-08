#!/bin/bash

source common.sh

PACKAGE_DIR=$PORTACLE_DIR/package/
PACKAGE_VERS=$(git rev-parse --short HEAD)
PACKAGE_FILE=$PLATFORM-portacle-$PACKAGE_VERS

function win-translate-names() {
    local result=()
    for file in "$@"; do
        result+=($(cygpath -w "$file"))
    done
    echo "${result[@]}"
}

function create-package() {
    local file="$1"
    local dir=$(dirname "$file")
    mkdir -p "$dir"
    cd "$PORTACLE_DIR"
    local files=($(discover-files "."))

    case "$PLATFORM" in
        win) local winfile=$(win-translate-names "$file")
             local winfiles=($(win-translate-names "${files[@]}"))
             eexit "THIS DOES NOT DO WHAT YOU WANT YET BECAUSE I CAN'T FUCKING GET THE 7ZIP COMMAND LINE OPTIONS TO WORK."
             "/c/Program Files/7-zip/7z.exe" a -t7z "$winfile.exe" -m0=LZMA2 -mmt2 -sfx7z.sfx -aoa -r -snh -snl -ssw -y "${winfiles[@]}" \
                   || eexit "Could not create package."
             ;;
        *)   tar -cJf "$file.tar.xz" "${files[@]}" \
                 || eexit "Could not create package."
             ;;
    esac
}

function discover-files() {
    ls -rt -d -1 "$1/"{*,.*} | egrep -v "build|package|/\\.{1,2}$"
}

function package-up() {
    create-package "$PACKAGE_DIR/$PACKAGE_FILE"
}

package-up
