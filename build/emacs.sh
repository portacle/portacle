#!/bin/bash

readonly TAG=emacs-25.1
readonly REPOSITORY=git://git.savannah.gnu.org/emacs.git
readonly CONFIGURE_OPTIONS=(--without-jpeg
                            --without-tiff
                            --without-gif
                            --without-png
                            --without-rsvg
                            --without-imagemagick
                            --without-sound
                            --without-makeinfo
                            --without-gconf
                            --without-dbus
                            --with-x-toolkit=gtk2)

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
        mac) ./configure --prefix="$INSTALL_TARGET" --with-ns --disable-ns-self-contained "${CONFIGURE_OPTIONS[@]}" \
                   || eexit "Configure failed. Maybe some dependencies are missing?" ;;
        *)   ./configure --prefix="$INSTALL_TARGET" "${CONFIGURE_OPTIONS[@]}" \
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

    status 2 "Copying dependencies"
    ensure-dependencies $(find-binaries "$INSTALL_TARGET/")
}

main
