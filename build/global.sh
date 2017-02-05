#!/bin/bash

readonly TAG=
readonly REPOSITORY=
readonly SSL_CA=https://curl.haxx.se/ca/cacert-2016-04-20.pem
readonly NOTO_DL=https://noto-website.storage.googleapis.com/pkgs
readonly INCONSOLATA_DL=https://github.com/google/fonts/blob/master/ofl/inconsolata

###

readonly PROGRAM=global
source common.sh

function download() {
    mkdir -p "$SOURCE_DIR/fonts"
    curl -o "$SOURCE_DIR/noto.zip" "$NOTO_DL/NotoSans-hinted.zip"
    unzip "$SOURCE_DIR/noto.zip" -d "$SOURCE_DIR/fonts/"
    curl -o "$SOURCE_DIR/fonts/Inconsolata-Bold.ttf" "$INCONSOLATA_DL/Inconsolata-Bold.ttf"
    curl -o "$SOURCE_DIR/fonts/Inconsolata-Regular.ttf" "$INCONSOLATA_DL/Inconsolata-Regular.ttf"
    curl -o "$SOURCE_DIR/ca-bundle.crt" "$SSL_CA"
}

function install() {
    mkdir -p "$SHARED_LIB_DIR/"
    mkdir -p "$SHARED_BIN_DIR/"

    mkdir -p "$PORTACLE_DIR/all/ssl"
    ensure-installed "$PORTACLE_DIR/all/ssl/" "$SOURCE_DIR/ca-bundle.crt"
    ensure-installed "$PORTACLE_DIR/all/fonts/" "$SOURCE_DIR/fonts"/*
}

function clean() {
    cd "$SOURCE_DIR"
    git clean -fxd
}

main
