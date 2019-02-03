#!/bin/bash

readonly TAG=
readonly REPOSITORY=
readonly SSL_CA=https://curl.haxx.se/ca/cacert-2016-04-20.pem
readonly NOTO_DL=https://noto-website.storage.googleapis.com/pkgs
readonly ICON_FONTS=("material-design-icons.ttf" "weathericons.ttf" "octicons.ttf" "fontawesome.ttf" "file-icons.ttf" "all-the-icons.ttf")
readonly ICON_FONTS_DL=https://raw.githubusercontent.com/domtronn/all-the-icons.el/master/fonts/

###

readonly PROGRAM=global
source common.sh

function download() {
    mkdir -p "$SOURCE_DIR/fonts"
    curl -o "$SOURCE_DIR/ca-bundle.crt" "$SSL_CA" \
        || eexit "Failed to download SSL certificate bundle."
    curl -o "$SOURCE_DIR/noto-sans.zip" "$NOTO_DL/NotoSans-hinted.zip" \
        || eexit "Failed to download Noto font."
    curl -o "$SOURCE_DIR/noto-mono.zip" "$NOTO_DL/NotoMono-hinted.zip" \
        || eexit "Failed to download Noto font."
    for font in "${ICON_FONTS[@]+${ICON_FONTS[@]}}"; do
        curl -o "$SOURCE_DIR/fonts/$font" "$ICON_FONTS_DL/$font" \
             || eexit "Failed to download $font."
    done
}

function prepare() {
    unzip -o "$SOURCE_DIR/noto-sans.zip" -d "$SOURCE_DIR/fonts/" \
        || eexit "Failed to extract Noto font."
    unzip -o "$SOURCE_DIR/noto-mono.zip" -d "$SOURCE_DIR/fonts/" \
        || eexit "Failed to extract Noto font."
}

function install() {
    mkdir -p "$SHARED_LIB_DIR/"
    mkdir -p "$SHARED_BIN_DIR/"
    mkdir -p "$PORTACLE_DIR/all/ssl"
    mkdir -p "$PORTACLE_DIR/all/locale/"
    
    case "$PLATFORM" in
        lin) ensure-shared-libraries "$SHARED_LIB_DIR/" "libnss_"
             ensure-dependencies $SHARED_LIB_DIR/libnss_*
             localedef -f UTF-8 -i en_US "$PORTACLE_DIR/all/locale/en_US.UTF-8" \
                 || eexit "Failed to generate locale"
             case "$ARCH" in
                 64) cp -fv /lib64/ld-linux-*.so.* "$SHARED_LIB_DIR/ld-linux.so" \
                           || eexit "Failed to copy ld-linux.so" ;;
                 32) cp -fv /lib/ld-linux-*.so.* "$SHARED_LIB_DIR/ld-linux.so" \
                           || eexit "Failed to copy ld-linux.so" ;;
             esac ;;
        mac) ucp "/usr/share/locale/en_US.UTF-8" "$PORTACLE_DIR/all/locale/" \
                   || eexit "Failed to copy locale"
             ;;
        win) cp -fv "/usr/bin/gzip" "$SHARED_BIN_DIR/" \
                   || eexit "Failed to copy gzip"
             ;;
    esac

    ensure-installed "$PORTACLE_DIR/all/ssl/" "$SOURCE_DIR/ca-bundle.crt"
    ensure-installed "$PORTACLE_DIR/all/fonts/" "$SOURCE_DIR/fonts"/*
}

function clean() {
    cd "$SOURCE_DIR"
    git clean -fxd
}

main
