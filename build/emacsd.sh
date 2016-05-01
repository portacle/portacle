#!/bin/bash

readonly REPOSITORY=https://github.com/shinmera/.emacs

###

PROGRAM=emacsd
source common.sh
INSTALL_TARGET=$SCRIPT_DIR/../emacs/config/shinmera/

function install (){
    mkdir -p "$INSTALL_TARGET"
    cp -R -t "$INSTALL_TARGET" "$SOURCE_DIR/."
    ## We don't remove the .git directory so that you can update easily.
}

main
