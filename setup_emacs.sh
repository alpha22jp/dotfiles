#!/bin/bash
#-----------------------------------------------------------------------
#  Setup script for Emacs
#-----------------------------------------------------------------------

base_dir=$(cd $(dirname $0); pwd)

if [ `which emacs` ]; then
    emacs_version=`emacs --version | sed '2,$d; s/.* \([0-9]*\).*/\1/'`
    if [ $emacs_version -ge 24 ]; then
        mkdir -p ~/.emacs.d/lisp-local
        ln -sf {$base_dir,~}/.emacs.d/init.el
        ln -sf {$base_dir,~}/.emacs.d/custom.el
        ln -sf {$base_dir,~}/.emacs.d/lisp
        ln -sf {$base_dir,~}/.emacs.d/snippets
    else
        echo "Emacs version should be greater than 24"
    fi
else
    echo "Emacs not found"
fi

exit 0
