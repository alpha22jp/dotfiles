# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific aliases and functions
alias ls='ls -aF --color=auto'
alias ll='ls -al'
alias screen='screen -U -D -RR'
alias emacsc='emacsclient -n -c -a ""'
alias emacsq='emacsclient -e "(kill-emacs)"'

export PS1='\u@\h:\w\$ '

if [ "$TERM" == "screen" ]; then
    export PROMPT_COMMAND='echo -ne "\ek$(basename $(pwd))\e\\"'
fi
