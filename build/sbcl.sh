#!/bin/bash

readonly TAG=sbcl-1.3.16
readonly REPOSITORY=https://github.com/sbcl/sbcl.git
readonly DISABLED_FEATURES=":largefile :sb-ldb"
readonly ENABLED_FEATURES=":sb-thread :sb-safepoint :sb-thruption :sb-wtimer :sb-core-compression"

###

readonly PROGRAM=sbcl
source common.sh
readonly INSTALL_SOURCES=$INSTALL_DIR/share/src/

function prepare() {
    cd "$SOURCE_DIR"

    case "$PLATFORM" in
        mac) EXTRA_DISABLED=":sb-safepoint :sb-thruption :sb-wtimer" ;;
        *)   EXTRA_DISABLED="" ;;
    esac

    if ! system-has "sbcl"; then
        case "$PLATFORM" in
            win) local sbcl_path=( "/c/Program Files/Steel Bank Common Lisp/"*/ )
                 export PATH="${sbcl_path[0]}:$PATH" ;;
        esac
        system-has "sbcl" \
            || eexit "SBCL not found in your path."
    fi
    
    cat >customize-target-features.lisp <<EOF
(lambda (features)
  (flet ((enable (x) (pushnew x features))
         (disable (x) (setf features (remove x features))))
    (mapc #'enable '($ENABLED_FEATURES))
    (mapc #'disable '($DISABLED_FEATURES $EXTRA_DISABLED)))
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
    cd "$SOURCE_DIR"
    unset SBCL_HOME
    mkdir -p "$INSTALL_DIR" &>/dev/null
    INSTALL_ROOT="$INSTALL_DIR" sh install.sh \
        || eexit "Failed to install SBCL."
    mkdir -p "$INSTALL_SOURCES" &>/dev/null
    ## Can't use -t because of apple.
    # cp -R -t "$INSTALL_SOURCES" "$SOURCE_DIR/src" "$SOURCE_DIR/contrib"
    status 2 "Installing SBCL sources"
    cp -R "$SOURCE_DIR/src" "$INSTALL_SOURCES" \
        || eexit "Failed to copy SBCL sources."
    cp -R "$SOURCE_DIR/contrib" "$INSTALL_SOURCES" \
        || eexit "Failed to copy SBCL contribs."
    find "$INSTALL_SOURCES" \
         -name "*.fasl" -or \
         -name "*.o" -or \
         -name "*.log" -or \
         -name "*.so" -or \
         -name "a.out" -delete

    status 2 "Copying dependencies"
    ensure-dependencies $(find-binaries "$INSTALL_DIR/")
}

main
