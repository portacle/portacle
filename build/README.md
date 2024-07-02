## Linux
Install the build prerequisites:

1. SBCL, GIT, time, xsel, texinfo, curl
2. Your distribution's basis build stuff (`base-devel`, `build-essential`, etc).
3. Development files for `ncurses x11 xpm gtk2 zlib pcre pcre2 curl4 ssl gcrypt glfw3 liblzma`.

For Debian based systems, simply run: `apt-get install sbcl git time curl build-essential autoconf automake autogen autopoint libncurses-dev libx11-dev libxpm-dev libgtk2.0-dev zlib1g-dev libpcre2-dev libcurl4-gnutls-dev libssl-dev libgcrypt-dev libglfw3-dev libtool liblzma-dev texinfo xsel libzstd1-dev libgnutls-dev libglu1-mesa-dev golang libassuan-dev libksba-dev`

Proceed with the `General Procedure` section.

## Windows
Install the build prerequisites:

1. SBCL, download and install <https://sourceforge.net/projects/sbcl/files/sbcl/1.3.5/sbcl-1.3.5-x86-64-windows-binary.msi/download>
2. MSYS2, download and install <http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20160205.exe>
3. 7Zip, download and install <http://www.7-zip.org/a/7z1604-x64.exe>
4. 7ZipSFX, download and extract into `C:\Program Files\7-zip` <http://web.archive.org/web/20160311162325/http://7zsfx.info/files/7zsd_extra_160_2712.7z>
3. Open the MSYS shell and run `pacman -Sy pacman`
4. Reopen the shell and run `pacman -Syu`
5. Reopen the shell and run `pacman -Su`
6. Install these packages: `pacman -S unzip git curl base-devel mingw-w64-x86_64-toolchain mingw-w64-x86_64-xpm-nox mingw-w64-x86_64-pcre2 mingw-w64-x86_64-curl mingw-w64-x86_64-gnutls mingw-w64-x86_64-iconv mingw-w64-x86_64-libgcrypt mingw-w64-x86_64-glfw mingw-w64-x86_64-go mingw-w64-x86_64-autotools mingw-w64-x86_64-libassuan mingw-w64-x86_64-libksba mingw-w64-x86_64-npth`
7. Launch the MinGW64 shell (not the MSYS shell!)

Proceed with the `General Procedure` section.

## Mac OS X
Install the build prerequisites:

1. XCode and the XCode command line developer tools
2. MacPorts, Homebrew, or Fink
3. From one of those package managers, install `autoconf automake sbcl git gpg gettext pcre openssl libtool gnu-sed glfw libgcrypt pkg-config xz automake gnutls pcre2 libassuan texinfo go`, making sure they are in your `PATH`.
4. With brew, you need to link gettext: `brew link gettext --force`

Proceed with the `General Procedure` section.

Note that the `Portacle.app` in the source folder cannot be launched outside of it, so do not copy it outside the source tree.

## General Procedure
Using the appropriate terminal, simply run:

    git clone https://github.com/portacle/portacle
    cd portacle/build
    ./build.sh
    
This will generate a ready-to-deploy package. In case you are only doing this for your own local needs and don't want it packaged up, you can instead run:

    ./build.sh upgrade

You can use this again every time you'd like to upgrade the binary components of Portacle to a new version. Should you ever desire a packaged deal, this will do it for you:

    ./build.sh package

If you need a completely fresh start that will *delete everything that is not in a clean clone*, run the `clean` target or `refresh` if you want to build a complete package.

Each component being built has its own build script that you can run individually as well. They all accept the name of a stage to run, usually one of: `clean`, `download`, `prepare`, `build`, or `install`, defaulting to running all of them in that sequence. The components are:

* `asdf` -- The de-facto standard Common Lisp build system. Bundled because SBCL's internal one is not always new enough.
* `busybox` -- A self-contained distribution of coreutils used on Linux as an independent base.
* `dictionaries` -- Dictionary files for use by the spellchecker.
* `emacs` -- Emacs is a long-standing, massively extensible editor ideal for editing Lisp code.
* `emacsd` -- Since Emacs is extensible, we need some sensible configuration for it.
* `git` -- Developing anything without version control is madness. Besides it's an easy way to access other people's projects and update Portacle itself.
* `global` -- Files required in general, or minor parts that don't fit anywhere else in specific.
* `hunspell` -- A cross-platform spell checker utility.
* `launcher` -- The Portacle launcher binary, responsible for preparing the runtime environment and launching other applications.
* `quicklisp` -- The de-facto standard Common Lisp package manager. Bundled because getting by without it would be much less than simple.
* `sbcl` -- The best and most simple to build open source Common Lisp implementation.

The other scripts, `build`, `common`, and `package` are integral parts of the build system. The first is responsible for allowing a more convenient interface to building. The second contains all sorts of general functions and parts required by the various scripts. The third handles the packaging process and knows how to bundle things together for the various formats required for deployment.
