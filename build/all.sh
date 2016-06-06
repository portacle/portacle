#!/bin/bash

./sbcl.sh \
    && ./emacs.sh \
    && ./git.sh \
    && ./quicklisp.sh \
    && ./emacsd.sh
