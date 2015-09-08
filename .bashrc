# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific aliases and functions
if [ `uname` = Darwin ]; then
    export LSCOLORS="gxfxcxdxbxegedabagacad"
    alias ls="ls -aFG"
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

# Shell prompt setting
export PS1='\u@\h:\w\$ '

# For screen and tmux
if [ "$TERM" == "screen" ]; then
    export PROMPT_COMMAND='echo -ne "\ek$(basename $(pwd))\e\\"'
fi

# Source local definitions
if [ -f $HOME/.bashrc.local ]; then
    . $HOME/.bashrc.local
fi

# For node.js (nvm)
export NVM_DIR=$HOME/.nvm
if [ -s "$NVM_DIR/nvm.sh" ]; then
    source $NVM_DIR/nvm.sh
    nvm use 0.12 >/dev/null
fi

# For Haskell
if [ -d $HOME/.cabal/bin ]; then
    export PATH=$HOME/.cabal/bin:$PATH
fi

# For Ruby
if [ -d $HOME/.gem/ruby/2.2.0/bin ]; then
    export PATH=$HOME/.gem/ruby/2.2.0/bin:$PATH
fi
