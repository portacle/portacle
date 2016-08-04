#!/bin/bash

./global.sh "$@" \
    && ./sbcl.sh "$@" \
    && ./asdf.sh "$@" \
    && ./quicklisp.sh "$@" \
    && ./emacs.sh "$@" \
    && ./emacsd.sh "$@" \
    && ./git.sh "$@" \
    && ./aspell.sh "$@" \
    && ./aspell-dicts.sh "$@" \
    && ./package.sh
