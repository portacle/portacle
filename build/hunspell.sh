#!/bin/bash

readonly TAG=v1.7.0
readonly REPOSITORY=https://github.com/hunspell/hunspell
readonly CONFIGURE_OPTIONS=(--without-ui
                            --without-readline)

###

readonly PROGRAM=hunspell
source common.sh

function prepare() {
    cd "$SOURCE_DIR"
    autoreconf -vfi \
        || eexit "Failed to generate configure. Maybe some dependencies are missing?"
    ./configure --prefix="$INSTALL_DIR" "${CONFIGURE_OPTIONS[@]}" \
        || eexit "Configure failed. Maybe some dependencies are missing?"
}

function build() {
    cd "$SOURCE_DIR"
    make -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function install() {
    cd "$SOURCE_DIR"
    make install \
        || eexit "The install failed. Please check the output for error messages."

    status 2 "Copying dependencies"
    ensure-dependencies $(find-binaries "$INSTALL_DIR/")

    case "$PLATFORM" in
        ## Bloody dylib shit
        mac) local files=(analyze chmorph hunspell hunzip)
             for file in "${files[@]}"; do
                 mac-fixup-dependencies "$INSTALL_DIR/bin/$file" "/usr/local/|libhunspell";
             done
             mac-fixup-lib-dependencies
             ;;
    esac
}

main
