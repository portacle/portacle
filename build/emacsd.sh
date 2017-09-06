#!/bin/bash

readonly TAG=
readonly REPOSITORY=https://github.com/portacle/emacsd.git

###

PROGRAM=emacsd
source common.sh
INSTALL_DIR=$PORTACLE_DIR/all/emacsd/portacle

function install (){
    mkdir -p "$INSTALL_DIR" \
        || eexit "Failed to create $INSTALL_DIR"
    cp -Rf "$SOURCE_DIR/." "$INSTALL_DIR" \
        || eexit "Failed to copy sources to $INSTALL_DIR"
    ## We don't remove the .git directory so that you can update easily.

    status 2 "Now launching emacs for the first time init."
    status 2 "It will download the necessary libraries and compile them."
    status 2 "Please be patient, it should automatically close when it is done."

    case "$PLATFORM" in
        win) $PORTACLE_DIR/$PLATFORM/bin/emacs -f "portacle-recompile" -f "kill-emacs" \
                   || eexit "Failed to perform first-launch." ;;
        mac) script -q /dev/null $PORTACLE_DIR/$PLATFORM/bin/emacs -f "portacle-recompile" -f "kill-emacs" -nw \
                   || eexit "Failed to perform first-launch." ;;
        lin) script -e -q -c "$PORTACLE_DIR/$PLATFORM/bin/emacs -f portacle-recompile -f kill-emacs -nw" /dev/null \
                   || status 2 "Warning: emacs setup might have failed." ;;
    esac
}

main
