## Linux
Install the build prerequisites:

1. SBCL
2. GIT
3. Your distribution's basis build stuff (`base-devel`, `build-essential`, etc).
4. Development files for `ncurses x11 xpm gtk2.0 zlib pcre curl`.

For Debian based systems, simply run: `apt-get install sbcl git build-essential autoconf automake libncurses-dev libx11-dev libxpm-dev libgtk2.0-dev zlib1g-dev libpcre-dev libcurl4-gnutls-dev`

Proceed with the `General Procedure` section.

## Windows
Install the build prerequisites:

1. SBCL, download and install https://sourceforge.net/projects/sbcl/files/sbcl/1.3.5/sbcl-1.3.5-x86-64-windows-binary.msi/download
2. MSYS2, download and install http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20160205.exe
3. Open the MSYS shell and run `pacman -Sy pacman`
4. Reopen the shell and run `pacman -Syu`
5. Reopen the shell and run `pacman -Su`
6. Install these packages: `pacman -S git base-devel mingw-w64-x86_64-toolchain mingw-w64-x86_64-xpm-nox mingw-w64-x86_64-pcre mingw-w64-x86_64-curl mingw-w64-x86_64-gnutls`
7. Launch the MinGW64 shell (not the MSYS shell!) and do `export PATH="/c/Program Files/Steel Bank Common Lisp/1.3.5/:$PATH"`

Proceed with the `General Procedure` section.

The `portacle.exe` file included in the root directory is created from the `portacle.bat` file using <http://www.f2ko.de/en/b2e.php>. This is done primarily to avoid creating a command prompt window and secondarily to allow adding an icon to the launcher.

## Mac OS X
Install the build prerequisites:

1. XCode and the XCode command line developer tools
2. MacPorts, Homebrew, or Fink
3. From one of those package managers, install `autoconf automake sbcl git`, making sure they are in your `PATH`.

Proceed with the `General Procedure` section.

Note that the `Portacle.app` in the source folder cannot be launched outside of it, so do not copy it outside the source tree.

## General Procedure
Using the appropriate terminal, simply run:

    git clone https://github.com/Shinmera/portacle
    cd portacle/build
    ./all.sh
    
The root portacle folder should now be built for the given system. You can ZIP up all the files except for the `build` folder and be ready to go. Things should be able to coexist with each other just fine, so you should be able to build for multiple systems with the same directory too.
