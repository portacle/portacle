This is a document describing my (apparently for naught) attempt to compile Emacs statically on an Arch system.

# Dependencies
First, build the necessary libs with static libs incldued. Arch does not ship those by default for no to me discernible reason.

    ./arch-build-static.sh libx11 libxau libxrandr libxinerama libxrender libxcb libxi libxcursor libxcomposite libxdamage libxfixes libxext libxpm libpng libffi  libxdmcp libthai libdatrie atk gdk-pixbuf2 fontconfig freetype2 gtk2 glib2 pango pixman harfbuzz pcre expat libxft libotf m17n-lib libtasn1 nettle libxml2 gmp
  
The following require manual intervention.

    TARGETS="copy prepare" ./arch-build-static.sh cairo

Edit `PKGBUILD`, replace `mesa-libgl` with `libgl`

    TARGETS="compile install" ./arch-build-static.sh cairo
    TARGETS="copy prepare" ./arch-build-static.sh bzip2

Edit `PKGBUILD`, add to `package()`: `install -m755 libbz2.a $pkgdir/usr/lib`

    TARGETS="compile install" ./arch-build-static.sh bzip2
    TARGETS="copy prepare" ./arch-build-static.sh ncurses

Edit `PKGBUILD`, add to `configure`: `--with-pkg-config-libdir=/usr/lib/pkgconfig`, add to `package()`: `ln -s libncursesw.a "$pkgdir"/usr/lib/libncurses.a`
                          `ln -s libncursesw.a "$pkgdir"/usr/lib/libcurses.a`

    TARGETS="compile install" ./arch-build-static.sh ncurses
    TARGETS="copy prepare" ./arch-build-static.sh xz

Edit `PKGBUILD`, remove from `configure`:  `--enable-werror`

    TARGETS="compile install" ./arch-build-static.sh xz
    TARGETS="copy prepare" ./arch-build-static.sh gnutls

Edit `PKGBUILD`, change `configure`: remove the last line, add `--without-p11-kit`

    TARGETS="compile install" ./arch-build-static.sh gnutls
    ./arch-build-static.sh graphite

Edit `src/graphite2-1.3.8/src/CMakeLists.txt`, change `add_library(graphite2 SHARED` to `add_library(graphite2 STATIC`, comment out: `nolib_test(stdc++ $<TARGET_SONAME_FILE:graphite2>)`

    cd src/build/
    cmake ../graphite2-1.3.8
    make
    sudo cp src/libgraphite.a /usr/lib/

# Emacs
Let's try this shit. Clone emacs, yaddayadda.

    cd emacs
    ./autogen.sh
    CFLAGS="-static" LDFLAGS="-static" LIBS="-lgnutls -lnettle -lhogweed -ltasn1 -lgtk-x11-2.0 -lgdk-x11-2.0 -lgdk_pixbuf-2.0 -lgio-2.0 -lXdamage -lpangocairo-1.0 -lpangoft2-1.0 -lpangoxft-1.0 -lpango-1.0 -lgobject-2.0 -lgmodule-2.0 -lglib-2.0 -lm17n -lm17n-core -lm17n-flt -lxml2 -latk-1.0 -lpcre -lresolv -lcairo -lfontconfig -lfreetype -lotf -lXdmcp -lpng -lpixman-1 -lharfbuzz -lgraphite2 -lXpm -lXrandr -lXinerama -lXcomposite -lXfixes -lXcursor -lXrender -lXext -lXi -lXft -lX11-xcb -lX11 -lxcb -lXdmcp -lXau -lexpat -lgmp -lm -lffi -lbz2 -lz -lpthread -ldl -lrt -lncurses -lthai -ldatrie -llzma" ./configure --without-jpeg --without-tiff --without-gif --without-png --without-rsvg --without-imagemagick --without-sound --without-makeinfo --with-x-toolkit=gtk2 --without-gconf --without-dbus

Edit `src/Makefile:597` change `$(LIBES)` to `$(LIBS)`

    CFLAGS="-static" LDFLAGS="-static" LIBS="-lgnutls -lnettle -lhogweed -ltasn1 -lgtk-x11-2.0 -lgdk-x11-2.0 -lgdk_pixbuf-2.0 -lgio-2.0 -lXdamage -lpangocairo-1.0 -lpangoft2-1.0 -lpangoxft-1.0 -lpango-1.0 -lgobject-2.0 -lgmodule-2.0 -lglib-2.0 -lm17n -lm17n-core -lm17n-flt -lxml2 -latk-1.0 -lpcre -lresolv -lcairo -lfontconfig -lfreetype -lotf -lXdmcp -lpng -lpixman-1 -lharfbuzz -lgraphite2 -lXpm -lXrandr -lXinerama -lXcomposite -lXfixes -lXcursor -lXrender -lXext -lXi -lXft -lX11-xcb -lX11 -lxcb -lXdmcp -lXau -lexpat -lgmp -lm -lffi -lbz2 -lz -lpthread -ldl -lrt -lncurses -lthai -ldatrie -llzma" make -j 12

Enjoy the segfault

In case you're wondering where those LIBS come from, I had to figure those out and their order on my own by tediously recompiling and reconfiguring things until it didn't error any more. Until the segfault of course, at which point I gave up and decided to forget about this shit because it's not worth the pain.
