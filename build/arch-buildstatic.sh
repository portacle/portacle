#!/bin/bash

DEFAULT_TARGETS=(clean copy prepare compile install)
TARGETS=(${TARGETS:-"${DEFAULT_TARGETS[@]}"})
BUILDROOT=${BUILDROOT:-"/tmp/portacle-buildstatic/"}
CPUS=${CPUS:-12}

function eexit() {
    echo -e "$@" >&2
    exit 1
}

function status() {
    echo -e "==> $@"
}

function update-abs() {
    sudo abs
}

function file-contains() {
    local search="$1"
    local file="$2"
    grep -q -e "$search" "$file"
}

function augment-options() {
    local pkgbuild="$1"
    sed -i 's/options=(/options=(makeflags staticlibs /g' "$pkgbuild"
}

function add-options() {
    local pkgbuild="$1"
    echo "options=(makeflags staticlibs)" >> "$pkgbuild"
}

function fixup-pkgbuild() {
    local pkgbuild="$1"
    if file-contains "^\\s*options=" "$pkgbuild"; then
        augment-options "$pkgbuild"
    else
        add-options "$pkgbuild"
    fi

    if file-contains "--disable-static" "$pkgbuild"; then
        sed -i 's,--disable-static,--enable-static,g' "$pkgbuild"
    elif file-contains "\\./configure" "$pkgbuild"; then
        sed -i 's,./configure,./configure --enable-static,g' "$pkgbuild"
    fi

    
}

function clean() {
    local package="$1"
    status "[$package] Cleaning up"
    rm "$BUILDROOT/$package/PKGBUILD" &> /dev/null
    return 1
}

function copy() {
    local package="$1"
    status "[$package] Copying files"
    mkdir -p "$BUILDROOT" \
        && cp -R /var/abs/*/$package "$BUILDROOT/" \
            || eexit "Failed to copy package."
}

function prepare() {
    local package="$1"
    status "[$package] Preparing build"
    fixup-pkgbuild "$BUILDROOT/$package/PKGBUILD" \
        || eexit "Failed to prepare package."
    cd "$BUILDROOT/$package"
    makepkg -o -s --skipinteg --noextract \
        || eexit "Failed to prepare package."
}

function compile() {
    local package="$1"
    if [ -z "$FORCE" ] && compgen -G "$BUILDROOT/$package/$package-*.tar.xz"; then
        status "[$package] Skipping compile."
    else
        status "[$package] Compiling"
        cd "$BUILDROOT/$package" \
            && MAKEFLAGS="-j $CPUS" makepkg -f --skipinteg \
                || eexit "Failed to compile package."
    fi
}

function install() {
    local package="$1"
    status "[$package] Installing"
    cd "$BUILDROOT/$package" \
        && sudo pacman -U "$BUILDROOT/$package/$package"*"x86_64"*".tar.xz" --noconfirm \
            || eexit "Failed to install package."
}

function info() {
    echo -e "Build Info:
Running targets:  ${TARGETS[@]}
Build root:       $BUILDROOT
Threads used:     $CPUS

"
}

function build-package() {
    local package="$1"

    clean-package "$package"
    copy-package "$package"
    prepare-package "$package"
    compile-package "$package"
}

function main() {
    info
    update-abs
    [ -z "$1" ] && eexit "Please specify at least one package to build."

    for target in "${TARGETS[@]}"; do
        for package in "$@"; do
            "$target" "$package"
        done
    done
}

main "$@"

