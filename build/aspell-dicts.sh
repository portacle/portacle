#!/bin/bash

readonly TAG=1.0.0
readonly REPOSITORY=https://github.com/shinmera/aspell-dicts
readonly ARCHIVES=https://github.com/Shinmera/aspell-dicts/releases/download/$TAG/
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

## Windows gets special treatment because life is too short to figure out
## how to get it to build the dictionary files on its own.
case "$PLATFORM" in
    win)
        function download() {
            export CURL_CA_BUNDLE=/usr/ssl/certs/ca-bundle.crt
            mkdir -p "$SOURCE_DIR"
            for dict in "${ALLDICTS[@]}"; do
                curl -L -o "$SOURCE_DIR/$dict.zip" "$ARCHIVES/aspell-dicts-$dict.zip" \
                    || eexit "Failed to download $dict from $ARCHIVES/aspell-dicts-$dict.zip"
            done
        }

        function prepare() {
            echo "Skipping prepare"
        }

        function build() {
            echo "Skipping build"
        }

        function install() {
            cd "$SOURCE_DIR"
            for dict in "${ALLDICTS[@]}"; do
                unzip -u "$SOURCE_DIR/$dict.zip" -d "$INSTALL_TARGET" \
                      || eexit "Failed to extract $dict"
            done
        }
        ;;
esac


main
