#!/bin/sh
readonly SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
readonly CERT_NAME="portacle"

if security find-identity -p codesigning -v | grep "$CERT_NAME"; then
    echo "Installing self-signed portacle certificate. Please confirm."
    security add-trusted-cert -d -r trustRoot -k "$HOME/Library/Keychains/login.keychain" "$SCRIPT_DIR/all/portacle.cer"
fi

echo "Clearing security attribute on all files..."
exes=( $(find "$SCRIPT_DIR/mac/" -perm +111 -type f) )
libs=( $(find "$SCRIPT_DIR/mac/" -name '*.dylib' ) )
xattr -cr "${exes[@]}" "${libs[@]}" "$SCRIPT_DIR/mac/sbcl/lib/sbcl/sbcl.core" "$SCRIPT_DIR/Portacle.app"

echo "All done. Try running Portacle now."
