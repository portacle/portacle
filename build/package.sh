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
    rm -rf "$INSTALL_TARGET/asdf/cache" "$INSTALL_TARGET/projects/"*
}

function install() {
    local package="$PORTACLE_DIR/$PROGRAM/$PACKAGE_FILE"
    local files=$(basename "$INSTALL_TARGET")

    mkdir -p $(dirname "$package")
    cd $(dirname "$INSTALL_TARGET")
    case "$PACKAGE_FORMAT" in
        sfx)
            local winfile=$(to-win-path "$package")
            local winfiles=($(to-win-path "$files"))
            local winconfig=$(to-win-path "$W7ZCONF")
            "$W7Z/7z.exe" a "$winfile.7z" -t7z -m0=LZMA2 -mx9 "-mmt$MAXCPUS" -aoa -r -snh -snl -ssw -y -- "${winfiles[@]}" \
                || eexit "Could not create package."
            cat "$W7Z/$W7ZSFX" "$W7ZCONF" "$package.7z" > "$package.exe"
            rm "$winfile.7z"
            ;;
        zip)
            zip -ry9 "$package.zip" "${files[@]}" \
                || eexit "Could not create package."
            ;;
        xz)
            XZ_DEFAULTS="-T $MAXCPUS" tar -cJf "$package.tar.xz" "${files[@]}" \
                || eexit "Could not create package."
            ;;
    esac
}

main
