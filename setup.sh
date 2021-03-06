#!/bin/bash
#-----------------------------------------------------------------------
#  Setup script for dotfiles
#-----------------------------------------------------------------------

base_dir=$(cd $(dirname $0); pwd)

make_symlink()
{
    target=$1
    name=$2
    if [ -f $target -a ! -L $target ]; then
        echo "$file alyready exists, rename it to $target.org"
        mv $target{,.org}
    fi
    ln -sf $base_dir/$target $name
}

files=(
    ".agignore"
    ".aspell.conf"
    ".bash_profile"
    ".bashrc"
    ".gemrc"
    ".gitconfig"
    ".git-completion.bash"
    ".git-prompt.sh"
    ".npmrc"
    ".tmux.conf"
    ".screenrc"
    )

cd ~

for file in ${files[@]}; do
    make_symlink $file $file
done
make_symlink ".gitconfig.linux" ".gitconfig.os"
make_symlink "vscode/settings.json" ".config/Code/User/settings.json"
make_symlink "vscode/keybindings.json" ".config/Code/User/keybindings.json"
