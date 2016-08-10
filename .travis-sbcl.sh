set -e

readonly URL=http://prdownloads.sourceforge.net/sbcl/sbcl-1.2.13-x86-64-linux-binary.tar.bz2
readonly INSTALL_DIR=$HOME/.sbcl/
readonly BUILD_DIR=$HOME/.prep/

function download() {
    mkdir -p "$BUILD_DIR"
    curl -o "$BUILD_DIR/sbcl.tar.bz2" -L "$URL"
    tar -C "$BUILD_DIR" --strip-components=1 -j -xc "$BUILD_DIR/sbcl.tar.bz2"
}

function install() {
    cd "$BUILD_DIR"
    INSTALL_ROOT="$INSTALL_DIR" bash install.sh
    export PATH="$INSTALL_DIR:$PATH"
}

function main() {
    download
    install
}

main
