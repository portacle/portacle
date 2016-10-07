#!/bin/bash

readonly TAG=${PACKAGE_VERSION:-$(git describe --tags)}
readonly REPOSITORY=
readonly W7Z="/c/Program Files/7-zip/"
readonly W7ZSFX="7zsd_LZMA2.sfx"

##

readonly PROGRAM=package
source common.sh
PACKAGE_FILE=${PACKAGE_FILE:-$TAG/$PLATFORM-portacle}
INSTALL_TARGET=$PORTACLE_DIR/$PROGRAM/portacle
PACKAGE_FORMAT=$([[ $PLATFORM = "win" ]] && echo "sfx" || echo "xz")
W7ZCONF=$SCRIPT_DIR/7zsfx.conf

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
    git gc
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

    mkdir -p $(dirname "$package")
    cd $(dirname "$INSTALL_TARGET")
    case "$PACKAGE_FORMAT" in
        sfx)
            local winfile=$(win-translate-paths "$package")
            local winfiles=($(win-translate-paths "$files"))
            local winconfig=$(win-translate-paths "$W7ZCONF")
            "$W7Z/7z.exe" a -t7z "$winfile.7z" -m0=LZMA2 -mmt2 -aoa -r -snh -snl -ssw -y "${winfiles[@]}" \
                || eexit "Could not create package."
            cat "$W7Z/$W7ZSFX" "$W7ZCONF" "$package.7z" > "$package.exe"
            rm "$winfile.7z"
            ;;
        zip)
            zip -ry9 "$package.zip" "${files[@]}" \
                || eexit "Could not create package."
            ;;
        xz)
            tar -cJf "$package.tar.xz" "${files[@]}" \
                || eexit "Could not create package."
            ;;
    esac
}

main
