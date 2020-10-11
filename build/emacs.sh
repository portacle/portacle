#!/bin/bash

readonly TAG=emacs-27.1
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
                            --without-xml2
                            --without-makeinfo
                            --with-x-toolkit=gtk2)

###

readonly PROGRAM=emacs
source common.sh

function prepare() {
    cd "$SOURCE_DIR"
    
    ./autogen.sh \
        || eexit "Failed to generate configure. Maybe some dependencies are missing?"
    case "$PLATFORM" in
        mac) ./configure --prefix="$INSTALL_DIR" --with-ns --disable-ns-self-contained "${CONFIGURE_OPTIONS[@]}" \
                   || eexit "Configure failed. Maybe some dependencies are missing?" ;;
        *)   ./configure --prefix="$INSTALL_DIR" "${CONFIGURE_OPTIONS[@]}" \
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
    # Ensure we clear out previous install
    rm -R "$INSTALL_DIR/"
    
    make install datadir="$INSTALL_DIR/share/" \
        || eexit "The install failed. Please check the output for error messages."

    status 2 "Copying dependencies"
    ensure-dependencies $(find-binaries "$INSTALL_DIR/")

    cp "$SCRIPT_DIR/config/site-start.el" "$INSTALL_DIR/share/emacs/site-lisp/site-start.el" \
        || eexit "Couldn't copy site file to correct site-lisp dir"

    case "$PLATFORM" in
        lin) cp "/usr/bin/xsel" "$SHARED_BIN_DIR/" \
                || eexit "Could not find xsel."
             ensure-dependencies "$SHARED_BIN_DIR/xsel"
             ;;
        mac) mac-fixup-dependencies "$INSTALL_DIR/bin/emacs"
             mac-fixup-dependencies "$INSTALL_DIR/bin/$TAG"
             mac-fixup-lib-dependencies
             ;;
    esac
}

main
