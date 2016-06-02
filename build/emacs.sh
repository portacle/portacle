#!/bin/bash

## This is not the latest, but 25 breaks minibuffer completion
## with ECB, rendering it unusable.
## Refer to https://github.com/ecb-home/ecb/issues/10
readonly TAG=emacs-25.0.94
readonly REPOSITORY=git://git.savannah.gnu.org/emacs.git
readonly CONFIGURE_OPTIONS=" --without-jpeg --without-tiff --without-gif --without-png --without-rsvg --without-imagemagick --without-sound --without-makeinfo --with-x-toolkit=gtk2"

###

readonly PROGRAM=emacs
source common.sh
readonly SHARE_TARGET=$SCRIPT_DIR/../$PROGRAM/share/

function prepare() {
    cd "$SOURCE_DIR"
    case "$TAG" in
        emacs-24*) git am < "$SCRIPT_DIR/fix-mingw64.patch" \
                         || eexit "Failed to apply mingw patch." ;;
    esac
    
    ./autogen.sh \
        || eexit "Failed to generate configure. Maybe some dependencies are missing?"
    case "$PLATFORM" in
        mac) ./configure --prefix="$INSTALL_TARGET" --with-ns --disable-ns-self-contained $CONFIGURE_OPTIONS \
                   || eexit "Configure failed. Maybe some dependencies are missing?" ;;
        *)   ./configure --prefix="$INSTALL_TARGET" $CONFIGURE_OPTIONS \
                   || eexit "Configure failed. Maybe some dependencies are missing?" ;;
    esac
}

function build() {
    cd "$SOURCE_DIR"
    make -j $MAXCPUS \
        || eexit "The build failed. Please check the output for error messages."
}

function install() {
    cd "$SOURCE_DIR"
    make install datadir="$SHARE_TARGET" \
        || eexit "The install failed. Please check the output for error messages."

    case "$PLATFORM" in
        win) cp "/mingw64/bin/libwinpthread-1.dll" "$INSTALL_TARGET/bin/" \
                   || eexit "Failed to copy libwinpthread-1.dll" ;;
    esac
}

main
