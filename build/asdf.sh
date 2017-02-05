#!/bin/bash

readonly TAG=3.1.7.5
readonly REPOSITORY=https://gitlab.common-lisp.net/asdf/asdf.git

###

readonly PROGRAM=asdf
source common.sh

function build() {
    cd "$SOURCE_DIR"
    make
    cd "$SOURCE_DIR/build/"
    cat >compile.lisp <<EOF
(cl:compile-file "asdf.lisp" :output-file "asdf.fasl")
(sb-ext:exit :code 0)
EOF
    "$SHARED_BIN_DIR/sbcl" --script compile.lisp \
        || eexit "Failed to compile ASDF."
}

function install() {
    ensure-installed "$INSTALL_DIR" "$SOURCE_DIR/build/asdf.lisp" "$SOURCE_DIR/build/asdf.fasl" \
        || eexit "Failed to copy ASDF sources."
}

main
