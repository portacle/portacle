#!/bin/bash

readonly TAG=
readonly REPOSITORY=
readonly SSL_CA=https://curl.haxx.se/ca/cacert-2016-04-20.pem
readonly NOTO_DL=https://noto-website.storage.googleapis.com/pkgs

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
    
    case "$PLATFORM" in
        lin) cp -fv "/lib64/ld-linux-x86-64.so.2" "$SHARED_LIB_DIR/" \
                   || eexit "Failed to copy ld-linux.so" ;;
    esac

    mkdir -p "$PORTACLE_DIR/all/ssl"
    ensure-installed "$PORTACLE_DIR/all/ssl/" "$SOURCE_DIR/ca-bundle.crt"
    ensure-installed "$PORTACLE_DIR/all/fonts/" "$SOURCE_DIR/fonts"/*
}

function clean() {
    cd "$SOURCE_DIR"
    git clean -fxd
}

main
