# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# dircolors
eval `dircolors -b`

# User specific aliases and functions
alias ls='ls -aF --color=auto'
alias ll='ls -al'
alias screen='screen -U -D -RR'
alias emacsc='emacsclient -n -c -a ""'
alias emacsq='emacsclient -e "(kill-emacs)"'

# For debug
ulimit -c unlimited

export PS1='\u@\h:\w\$ '

# For screen and tmux
if [ "$TERM" == "screen" ]; then
    export PROMPT_COMMAND='echo -ne "\ek$(basename $(pwd))\e\\"'
fi
