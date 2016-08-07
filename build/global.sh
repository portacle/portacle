#!/bin/bash

readonly TAG=
readonly REPOSITORY=
readonly SSL_CA=https://curl.haxx.se/ca/cacert-2016-04-20.pem

###

readonly PROGRAM=global
source common.sh
INSTALL_TARGET="$SHARED_DIR/lib"

function build() {
    mkdir -p "$SOURCE_DIR"
    case "$PLATFORM" in
        lin) gcc -std=c99 -fPIC -shared -o "$SOURCE_DIR/ld-wrap.so" "$SOURCE_DIR/ld-wrap.c" -ldl \
                   || eexit "Failed to build ld-wrap.so"
             cp -fuv "/lib64/ld-linux-x86-64.so.2" "$SOURCE_DIR/ld-linux.so" \
                 || eexit "Failed to copy ld-linux.so"
             ;;
    esac
}

function install() {
    mkdir -p "$INSTALL_TARGET/"
    case "$PLATFORM" in
        lin) ensure-installed "$INSTALL_TARGET/" "$SOURCE_DIR/ld-wrap.so" "$SOURCE_DIR/ld-linux.so" ;;
    esac

    mkdir -p "$SHARED_DIR/ssl"
    curl -o "$SHARED_DIR/ssl/ca-bundle.crt" "$SSL_CA"
}

main
