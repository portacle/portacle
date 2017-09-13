#!/bin/bash

readonly TAG=v1.6.1
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
             local path=$(otool -L "$INSTALL_DIR/bin/hunspell" | grep libhunspell | awk '{print $1}')
             local filename=$(basename "$path")
             for file in "${files[@]}"; do
                 status 2 "Fixing dylib entries for $file"
                 install_name_tool -change "$path" "@loader_path/../lib/$filename" "$INSTALL_DIR/bin/$file"
             done
             ;;
    esac
}

main
