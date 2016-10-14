#!/bin/bash

readonly TAG=
readonly REPOSITORY=
readonly SSL_CA=https://curl.haxx.se/ca/cacert-2016-04-20.pem
readonly NOTO_DL=https://noto-website.storage.googleapis.com/pkgs
readonly INCONSOLATA_DL=https://github.com/google/fonts/blob/master/ofl/inconsolata

###

readonly PROGRAM=global
source common.sh
INSTALL_TARGET="$SHARED_DIR/lib"

function download() {
    mkdir -p "$SOURCE_DIR/fonts"
    curl -o "$SOURCE_DIR/noto.zip" "$NOTO_DL/NotoSans-hinted.zip"
    unzip "$SOURCE_DIR/noto.zip" -d "$SOURCE_DIR/fonts/"
    curl -o "$SOURCE_DIR/fonts/Inconsolata-Bold.ttf" "$INCONSOLATA_DL/Inconsolata-Bold.ttf"
    curl -o "$SOURCE_DIR/fonts/Inconsolata-Regular.ttf" "$INCONSOLATA_DL/Inconsolata-Regular.ttf"
    curl -o "$SOURCE_DIR/ca-bundle.crt" "$SSL_CA"
}

function build() {
    mkdir -p "$SOURCE_DIR"
    case "$PLATFORM" in
        lin)
            gcc -o "$SOURCE_DIR/ld-wrap.so" -Wall -std=c99 -fPIC -shared -ldl -Wl,-init,init "$SOURCE_DIR/ld-wrap.c" \
                || eexit "Failed to build ld-wrap.so"
            cp -fuv "/lib64/ld-linux-x86-64.so.2" "$SOURCE_DIR/ld-linux.so" \
                || eexit "Failed to copy ld-linux.so"
            ;;
        win)
            gcc -o "$SOURCE_DIR/fontreg.exe" -Wall -std=c99 -mwindows "$SOURCE_DIR/fontreg.c" \
                || eexit "Failed to build fontreg.exe"
    esac   
}

function install() {
    mkdir -p "$INSTALL_TARGET/"
    case "$PLATFORM" in
        lin) ensure-installed "$INSTALL_TARGET/" "$SOURCE_DIR/ld-wrap.so" "$SOURCE_DIR/ld-linux.so" ;;
        win) ensure-installed "$SHARED_DIR/bin/" "$SOURCE_DIR/fontreg.exe" ;;
    esac

    mkdir -p "$SHARED_DIR/ssl"
    ensure-installed "$SHARED_DIR/ssl/" "$SOURCE_DIR/ca-bundle.crt"
    ensure-installed "$SHARED_DIR/fonts/" "$SOURCE_DIR/fonts"/*
}

function clean() {
    cd "$SOURCE_DIR"
    git clean -fxd
}

main
