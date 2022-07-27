#!/bin/bash

readonly TAG=cp-5.3-4
readonly REPOSITORY=https://anongit.freedesktop.org/git/libreoffice/dictionaries.git
readonly DICTIONARIES=(en/en_GB
                       en/en_US
                       fr_FR/fr
                       it_IT/it_IT
                       es/es_ANY
                       pt_BR/pt_BR
                       pt_PT/pt_PT
                       de/de_DE)

###

readonly PROGRAM=dictionaries
source common.sh
INSTALL_DIR=$PORTACLE_DIR/all/$PROGRAM

function build() {
    mkdir -p "$SOURCE_DIR/extracted"
    for dictionary in "${DICTIONARIES[@]}"; do
        cp -v "$SOURCE_DIR/$dictionary"* "$SOURCE_DIR/extracted/"
    done
}

function install() {
    mkdir -p "$INSTALL_DIR" \
        || eexit "Failed to create $INSTALL_DIR"
    cp -rv "$SOURCE_DIR/extracted/." "$INSTALL_DIR" \
        || eexit "Failed to copy dictionaries."
}

main
