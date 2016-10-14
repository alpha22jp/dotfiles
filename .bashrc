# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# User specific aliases and functions
if [ `uname` = Darwin ]; then
    export LSCOLORS="gxfxcxdxbxegedabagacad"
    alias ls="ls -aFG"
    alias echo="gecho"
else
    eval `dircolors -b`
    alias ls='ls -aF --color=auto'
fi
alias ..='cd ..'
alias ll='ls -al'
alias screen='screen -U -D -RR'
alias emacsc='emacsclient -n -c -a ""'
alias emacsq='emacsclient -e "(kill-emacs)"'

# For debug
ulimit -c unlimited

# For screen and tmux
if [ "$TERM" == "screen" ]; then
    export PROMPT_COMMAND='echo -ne "\ek$(basename $(pwd))\e\\"'
fi

# For Git
if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.bash
fi
if [ -f ~/.git-prompt.sh ]; then
    GIT_PS1_SHOWDIRTYSTATE=1
    . ~/.git-prompt.sh
    PS1='\u@\h$(__git_ps1 "<<%s>>"):\w\$ '
fi

# Source local definitions
if [ -f $HOME/.bashrc.local ]; then
    . $HOME/.bashrc.local
fi
