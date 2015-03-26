#!/bin/sh
#-----------------------------------------------------------------------
#  Setup script for dotfiles
#-----------------------------------------------------------------------

symlink_to_dotfiles()
{
    file=$1
    if [ -f $file -a ! -L $file ]; then
        echo "$file alyready exists, rename it to $file.org"
        mv $file{,.org}
    fi
    ln -sf ~/dotfiles/$file $file
}

cd ~/ # ホームディレクトリに移動

#-----------------------------------------------------------------------
# ホームディレクトリに置かれるファイル
#-----------------------------------------------------------------------
files=(
    ".agignore"
    ".aspell.conf"
    ".bashrc"
    ".gemrc"
    ".gitconfig"
    ".tmux.conf"
    ".screenrc"
    )

for file in ${files[@]}; do
    symlink_to_dotfiles $file
done

#-----------------------------------------------------------------------
# Emacsの設定ファイル
#-----------------------------------------------------------------------

if [ `which emacs` ]; then
    emacs_version=`emacs --version | sed '2,$d; s/.* \([0-9]*\).*/\1/'`
    if [ $emacs_version -ge 23 ]; then
        ln -sf {~/dotfiles/,}.emacs.d/lisp
        ln -sf {~/dotfiles/,}.emacs.d/lisp-23
        symlink_to_dotfiles ".emacs.d/init.el"
    else
        echo "Emacs version should be greater than 23"
    fi
else
    echo "Emacs not found, skip emacs setting"
fi
