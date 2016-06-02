#!/bin/bash

readonly REPOSITORY=https://github.com/shinmera/.emacs

###

PROGRAM=emacsd
source common.sh
INSTALL_TARGET=$SCRIPT_DIR/../emacs/config/shinmera/

function install (){
    mkdir -p "$INSTALL_TARGET" \
        || eexit "Failed to create $INSTALL_TARGET"
    cp -R "$SOURCE_DIR/." "$INSTALL_TARGET" \
        || eexit "Failed to copy sources to $INSTALL_TARGET"
    ## We don't remove the .git directory so that you can update easily.
    echo ""
    echo ";;;; Now launching emacs for the first time init."
    echo ";; It will download the necessary libraries and compile them."
    echo ";; Please be patient, it should automatically close when it is done."

    case "$PLATFORM" in
        win) winroot=$({ cd "$SCRIPT_DIR/.." && pwd -W; } | sed 's|/|\\|g')
             cmd /c "$winroot\\emacs\\win\\emacs.bat -f kill-emacs" ;;
        *)   $SCRIPT_DIR/../emacs/$PLATFORM/emacs.sh -nw -f "kill-emacs" \
                 || eexit "Failed to perform first-launch." ;;
    esac
}

main
