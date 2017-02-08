#!/bin/bash

readonly TAG=${PACKAGE_VERSION:-$(git describe --tags)}
readonly REPOSITORY=
readonly W7Z="/c/Program Files/7-zip/"
readonly W7ZSFX="7zsd_LZMA2.sfx"

##

readonly PROGRAM=package
source common.sh
PACKAGE_FILE=${PACKAGE_FILE:-$TAG/$PLATFORM-portacle}
INSTALL_DIR=$PORTACLE_DIR/$PROGRAM/portacle
W7ZCONF=$SCRIPT_DIR/src/7zsfx.conf

case "$PLATFORM" in
    win) PACKAGE_FORMAT=${PACKAGE_FORMAT:-sfx} ;;
    lin) PACKAGE_FORMAT=${PACKAGE_FORMAT:-xz} ;;
    mac) PACKAGE_FORMAT=${PACKAGE_FORMAT:-dmg} ;;
esac

function prepare() {
    mkdir -p "$INSTALL_DIR"
    git gc --aggressive
}

function build() {
    local files=("$PORTACLE_DIR/$PLATFORM"
                 "$PORTACLE_DIR/all"
                 "$PORTACLE_DIR/config"
                 "$PORTACLE_DIR/.git"
                 "$PORTACLE_DIR/.gitignore"
                 "$PORTACLE_DIR/.portacle_root"
                 "$PORTACLE_DIR/portacle.svg")
    
    if system-has rsync; then
        rsync -Havz --delete "${files[@]}" "$INSTALL_DIR/"
    else
        rm -rf "$INSTALL_DIR"
        mkdir -p "$INSTALL_DIR"
        cp -Rfva "${files[@]}" "$INSTALL_DIR"
    fi

    # Copy launcher
    case "$PLATFORM" in
        win) cp -fv "$PORTACLE_DIR/portacle.exe" "$INSTALL_DIR/" ;;
        lin) cp -fv "$PORTACLE_DIR/portacle.desktop" "$PORTACLE_DIR/portacle.run" "$INSTALL_DIR/";;
        mac) cp -Rfv "$PORTACLE_DIR/Portacle.app" "$INSTALL_DIR/" ;;
    esac
}

function install() {
    local package="$PORTACLE_DIR/$PROGRAM/$PACKAGE_FILE"
    local files=$(basename "$INSTALL_DIR")

    mkdir -p $(dirname "$package")
    cd $(dirname "$INSTALL_DIR")
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
        dmg)
            local tmpdir="$PORTACLE_DIR/$PROGRAM/tmp/"
            mkdir -p "$tmpdir/portacle"
            rsync -az "$INSTALL_DIR/" "$tmpdir/portacle/"
            cp "$INSTALL_DIR/Portacle.app/Contents/Resources/.DS_Store" "$tmpdir/"
            hdiutil makehybrid -hfs -hfs-volume-name "Portacle" -hfs-openfolder "$tmpdir" "$tmpdir" -o "$package.tmp.dmg" \
                || eexit "Could not create package. (Failed to bundle)"
            hdiutil convert -format UDZO  -imagekey zlib-level=9 "$package.tmp.dmg" -o "$package.dmg" \
                || eexit "Could not create package. (Failed to compress)"
            rm -rf "$PACKAGE_DIR/tmp/portacle" "$package.tmp.dmg"
            ;;
        xz)
            XZ_DEFAULTS="-T $MAXCPUS" tar -cJf "$package.tar.xz" "${files[@]}" \
                || eexit "Could not create package."
            ;;
    esac
}

main
