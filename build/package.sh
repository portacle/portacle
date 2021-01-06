#!/bin/bash

readonly TAG=${PACKAGE_VERSION:-$(git describe --tags)}
readonly REPOSITORY=
readonly W7Z="/c/Program Files/7-zip/"
readonly W7ZSFX="7zsd_LZMA2.sfx"
readonly SIGN_KEY="9F5D99D8BAE57852AD4610028291D87D2FA7E888"
readonly CERT_CN="49U6SFW62Y"
readonly NOTAR_EMAIL="shirakumo@tymoon.eu"

##

readonly PROGRAM=package
source common.sh
PACKAGE_FILE=${PACKAGE_FILE:-$TAG/$PLATFORM-portacle}
INSTALL_DIR=$PORTACLE_DIR/$PROGRAM/portacle
SOURCE_DIR=$SCRIPT_DIR/src/

case "$PLATFORM" in
    win) PACKAGE_FORMAT=${PACKAGE_FORMAT:-sfx} ;;
    lin) PACKAGE_FORMAT=${PACKAGE_FORMAT:-xz} ;;
    mac) PACKAGE_FORMAT=${PACKAGE_FORMAT:-dmg} ;;
esac

function prepare() {
    mkdir -p "$INSTALL_DIR"
}

function sign() {
    local package="$1"
    local signature="$2"
    
    gpg --armor --output "$signature" -u "$SIGN_KEY" --detach-sig "$package"
}

function certify() {
    local exes=( $(find "$INSTALL_DIR/mac/" -perm +111 -type f) )
    local libs=( $(find "$INSTALL_DIR/mac/" -name '*.dylib' ) )
    local files=("${exes[@]}" "${libs[@]}")
    codesign -s "$CERT_CN" \
             --force \
             --deep \
             --options runtime \
             --timestamp \
             --entitlements "$SOURCE_DIR/entitlements.plist" \
             "${files[@]}" "$INSTALL_DIR/Portacle.app"
}

function notarize() {
    local package="$1"

    local ticket=$(xcrun altool --notarize-app \
                         --file "$package" \
                         --primary-bundle-id "org.shirakumo.portacle" \
                         --username "$NOTAR_EMAIL" \
                         --password "@keychain:AC_PASSWORD" \
                         --asc-provider "$CERT_CN"
                    | grep "RequestUUID"
                    | awk '{print $3}')
    [ -z "$ticket" ] \
        && eexit "Failed to notarize app."
    echo "$ticket" > $SCRIPT_DIR/.notarization-ticket
    status 2 "Notarization started under ticket $ticket"
}

function check-notarization() {
    local ticket=$(cat $SCRIPT_DIR/.notarization-ticket)
    [ -z "$ticket" ] \
        && eexit "No notarization ID known."

    xcrun altool \
          --username "$NOTAR_EMAIL" \
          --password "@keychain:AC_PASSWORD" \
          --notarization-info "$ticket"
}

function staple() {
    local package="$PORTACLE_DIR/$PROGRAM/$PACKAGE_FILE.dmg"
    
    xcrun stapler staple "$package"
}

function build() {
    local files=("$PORTACLE_DIR/$PLATFORM"
                 "$PORTACLE_DIR/all"
                 "$PORTACLE_DIR/config"
                 "$PORTACLE_DIR/.portacle_root")
    
    if system-has rsync; then
        rsync -Havz --delete "${files[@]}" "$INSTALL_DIR/"
    else
        rm -rf "$INSTALL_DIR"
        mkdir -p "$INSTALL_DIR"
        cp -Rfva "${files[@]}" "$INSTALL_DIR"
    fi

    # Create projects
    mkdir -p "$INSTALL_DIR/projects/"
    cp -fv "$SOURCE_DIR/projects.md" "$INSTALL_DIR/projects/README.md"

    # Create launcher
    case "$PLATFORM" in
        win) cp -fv "$SHARED_BIN_DIR/portacle.exe" "$INSTALL_DIR/"
             ;;
        lin) cp -fv "$SOURCE_DIR/portacle.desktop" "$INSTALL_DIR/"
             cp -fv "$SOURCE_DIR/portacle.run" "$INSTALL_DIR/"
             cp -fv "$SOURCE_DIR/portacle.svg" "$INSTALL_DIR/"
             ;;
        mac) mkdir -p "$INSTALL_DIR/Portacle.app/Contents/MacOS/"
             mkdir -p "$INSTALL_DIR/Portacle.app/Contents/Resources/"
             cp -fv "$SOURCE_DIR/Info.plist" "$INSTALL_DIR/Portacle.app/Contents/"
             cp -fv "$SOURCE_DIR/portacle.icns" "$INSTALL_DIR/Portacle.app/Contents/Resources/"
             cp -fv "$SHARED_BIN_DIR/portacle" "$INSTALL_DIR/Portacle.app/Contents/MacOS/portacle"
             ;;
    esac

    case "$PLATFORM" in
        mac)
            if security find-identity -p codesigning -v | grep "$CERT_CN"; then
                status 2 "Signing all binaries..."
                certify
            else
                status 2 "Failed to find codesiging certificate, skipping certification."
            fi
            ;;
    esac
}

function install() {
    local package="$PORTACLE_DIR/$PROGRAM/$PACKAGE_FILE"
    local signature="$package.sig"
    local files=$(basename "$INSTALL_DIR")

    mkdir -p $(dirname "$package")
    cd $(dirname "$INSTALL_DIR")
    status 1 "Creating package $package"
    case "$PACKAGE_FORMAT" in
        sfx)
            package="$package.exe"
            local winfile=$(to-win-path "$package")
            local winfiles=($(to-win-path "$files"))
            "$W7Z/7z.exe" a "$winfile.7z" -t7z -m0=LZMA2 -mx9 "-mmt$MAXCPUS" -aoa -r -snh -snl -ssw -y -- "${winfiles[@]}" \
                || eexit "Could not create package."
            cat "$W7Z/$W7ZSFX" "$SOURCE_DIR/7zsfx.conf" "$package.7z" > "$package"
            rm "$winfile.7z"
            ;;
        zip)
            package="$package.zip"
            zip -ry9 "$package" "${files[@]}" \
                || eexit "Could not create package."
            ;;
        dmg)
            package="$package.dmg"
            local tmpdir="$PORTACLE_DIR/$PROGRAM/tmp/"
            mkdir -p "$tmpdir/portacle"
            rsync -az "$INSTALL_DIR/" "$tmpdir/portacle/"
            cp "$INSTALL_DIR/Portacle.app/Contents/Resources/.DS_Store" "$tmpdir/"
            hdiutil makehybrid -hfs -hfs-volume-name "Portacle" -hfs-openfolder "$tmpdir" "$tmpdir" -o "$package.tmp.dmg" \
                || eexit "Could not create package. (Failed to bundle)"
            hdiutil convert -format UDZO  -imagekey zlib-level=9 "$package.tmp.dmg" -o "$package" \
                || eexit "Could not create package. (Failed to compress)"
            rm -rf "$tmpdir" "$package.tmp.dmg"
            if security find-identity -p codesigning -v | grep "$CERT_CN"; then
                status 2 "Submitting for notarisation..."
                notarize "$package"
            else
                status 2 "Failed to find codesigning certificate, skipping notarisation."
            fi
            ;;
        xz)
            package="$package.tar.xz"
            XZ_DEFAULTS="-T $MAXCPUS" tar -cJf "$package" "${files[@]}" \
                || eexit "Could not create package."
            ;;
    esac
    if gpg --list-secret-keys "$SIGN_KEY"; then
        sign "$package" "$signature"
    fi
}

main
