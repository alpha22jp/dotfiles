#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Usage: copy_svn_modified_files.sh [src dir] [dest dir]"
    exit 0
fi

(cd $1; svn st -q) | sed -e 's/^M[\t ]*//' | (while read i; do cp {$1,$2}/$i; done)
