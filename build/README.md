## Linux

## Windows
Install the build prerequisites:

1. SBCL Download and install https://sourceforge.net/projects/sbcl/files/sbcl/1.3.5/sbcl-1.3.5-x86-64-windows-binary.msi/download
2. MINGW2 Download and install http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20160205.exe
3. Open the MSYS shell and run `pacman -Sy pacman`
4. Reopen the shell and run `pacman -Syu`
5. Reopen the shell and run `pacman -Su`
6. Finally, run: `pacman -S git base-devel mingw-w64-x86_64-toolchain mingw-w64-x86_64-xpm-nox`

Now, launch the MinGW64 shell (not the MSYS shell!) and do:

    export PATH="/c/Program Files/Steel Bank Common Lisp/1.3.5/:$PATH"
    
Adapt the version in the path for the appropriate one if necessary.
Finally, clone this repo to some place, enter the build directory, and let it rip.

    git clone https://github.com/Shinmera/portacle
    cd portacle/build
    ./all.sh
