#! /usr/bin/env bash

find ~/.emacs.d/elisp -iname "*.el" | xargs emacs -batch -f batch-byte-compile

