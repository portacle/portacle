#!/bin/sh
readonly SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
readonly CERT_NAME="portacle"

if security find-identity -p codesigning -v | grep "$CERT_NAME"; then
    echo "Installing self-signed portacle certificate. Please confirm."
    security add-trusted-cert -d -r trustRoot -k "$HOME/Library/Keychains/login.keychain" "$SCRIPT_DIR/all/portacle.cer"
fi

function clear() {
    $(xattr -cr "$1")
    $(codesign -s "$CERT_NAME" "$1")
}

echo "Self-signing all files with the portacle certificate..."
for file in $(find "$SCRIPT_DIR" -type f -perm +0111); do
    if [ "$(file $file | grep "Mach-O 64-bit executable" )" != "" ]; then
        clear "$file"
    fi
done

for file in $(find . -type f -name "*.dylib"); do
    clear "$file"
done

clear "$SCRIPT_DIR/Portacle.app"
clear "$SCRIPT_DIR/mac/sbcl/lib/sbcl/sbcl.core"
echo "All done. Try running Portacle now."
