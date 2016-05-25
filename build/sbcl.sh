#!/bin/bash

readonly TAG=sbcl-1.3.5
readonly REPOSITORY=https://github.com/sbcl/sbcl.git
readonly ENABLED_FEATURES=":sb-thread :sb-safepoint :sb-thruption :sb-wtimer :sb-core-compression"
readonly DISABLED_FEATURES=":largefile :sb-ldb"

###

readonly PROGRAM=sbcl
source common.sh
readonly INSTALL_SOURCES=$SCRIPT_DIR/../$PROGRAM/sources/

function prepare() {
    cd "$SOURCE_DIR"
    cat >customize-target-features.lisp <<EOF
(lambda (features)
  (flet ((enable (x) (pushnew x features))
         (disable (x) (setf features (remove x features))))
    (mapc #'enable '($ENABLED_FEATURES))
    (mapc #'disable '($DISABLED_FEATURES)))
  features)
EOF
}

function build() {
    cd "$SOURCE_DIR"
    export CFLAGS="${CFLAGS} -fno-omit-frame-pointer -D_GNU_SOURCE"
    sh make.sh \
        || eexit "Failed to build SBCL."
}

function install() {
    cd $SOURCE_DIR
    unset SBCL_HOME
    mkdir -p "$INSTALL_TARGET" &>/dev/null
    INSTALL_ROOT="$INSTALL_TARGET" sh install.sh \
        || eexit "Failed to install SBCL."
    mkdir -p "$INSTALL_SOURCES" &>/dev/null
    cp -R -t "$INSTALL_SOURCES" "$SOURCE_DIR/src" "$SOURCE_DIR/contrib"
    find "$INSTALL_SOURCES" \
         -name "*.fasl" -or \
         -name "*.o" -or \
         -name "*.log" -or \
         -name "*.so" -or \
         -name "a.out" -delete

    if [[ "$PLATFORM" == "win" ]]; then
        cp /mingw64/bin/zlib1.dll "$INSTALL_TARGET/bin/"
    fi
}

main
