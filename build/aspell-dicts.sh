#!/bin/bash

readonly TAG=1.0.0
readonly REPOSITORY=https://github.com/shinmera/aspell-dicts
readonly DICTS=( "en" )

###

readonly PROGRAM=aspell-dicts
source common.sh
INSTALL_TARGET=$PORTACLE_DIR/aspell/share

if [ "$DICTS" == "all" ]; then
    ALLDICTS=( "$SOURCE_DIR/"*/ )
else
    ALLDICTS="${DICTS[@]}"
fi

function prepare() {
    cd "$SOURCE_DIR"
    for dict in "${ALLDICTS[@]}"; do
        cd "$dict"
        ./configure --vars ASPELL="$SCRIPT_DIR/../aspell/$PLATFORM/aspell.sh" \
                    PREZIP="$SCRIPT_DIR/../aspell/$PLATFORM/bin/prezip-bin" \
            || eexit "Failed to configure $dict"
    done
}

function build() {
    cd "$SOURCE_DIR"
    for dict in "${ALLDICTS[@]}"; do
        cd "$dict"
        make \
            || eexit "Failed to build $dict"
    done
}

function install() {
    cd "$SOURCE_DIR"
    for dict in "${ALLDICTS[@]}"; do
        cd "$dict"
        make install \
            || eexit "Failed to install $dict"
    done
}

main
