#!/bin/sh
#-----------------------------------------------------------------------
#  Setup script for Emacs
#-----------------------------------------------------------------------

base_dir=$(cd $(dirname $0); pwd)

if [ `which emacs` ]; then
    emacs_version=`emacs --version | sed '2,$d; s/.* \([0-9]*\).*/\1/'`
    if [ $emacs_version -ge 23 ]; then
        mkdir -p ~/.emacs.d
        ln -sf {$base_dir,~}/.emacs.d/init.el
        ln -sf {$base_dir,~}/.emacs.d/lisp
        ln -sf {$base_dir,~}/.emacs.d/lisp-23
    else
        echo "Emacs version should be greater than 23"
    fi
else
    echo "Emacs not found"
fi

exit 0
