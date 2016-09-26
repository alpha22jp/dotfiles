#!/bin/bash
#-----------------------------------------------------------------------
#  Setup script for dotfiles
#-----------------------------------------------------------------------

base_dir=$(cd $(dirname $0); pwd)

symlink_to_dotfiles()
{
    file=$1
    if [ -f $file -a ! -L $file ]; then
        echo "$file alyready exists, rename it to $file.org"
        mv $file{,.org}
    fi
    ln -sf {$base_dir/,}$file
}

files=(
    ".agignore"
    ".aspell.conf"
    ".bashrc"
    ".gemrc"
    ".gitconfig"
    ".npmrc"
    ".tmux.conf"
    ".screenrc"
    )

cd ~

for file in ${files[@]}; do
    symlink_to_dotfiles $file
done
