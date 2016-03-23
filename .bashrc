# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

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

# For subversion
export SVN_EDITOR=emacsclient

# For node.js (nvm)
export NVM_DIR=$HOME/.nvm
if [ -s "$NVM_DIR/nvm.sh" ]; then
    source $NVM_DIR/nvm.sh
fi

# For Haskell
if [ -d $HOME/.cabal/bin ]; then
    export PATH=$HOME/.cabal/bin:$PATH
fi

# For Ruby
if [ -d $HOME/.gem/ruby/2.2.0/bin ]; then
    export PATH=$HOME/.gem/ruby/2.2.0/bin:$PATH
elif [ -d $HOME/.gem/ruby/2.0.0/bin ]; then
    export PATH=$HOME/.gem/ruby/2.0.0/bin:$PATH
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
