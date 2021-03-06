# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

if [ -z $BASH_PROFILE_LOADED ]; then
    export BASH_PROFILE_LOADED=1

    export LD_LIBRARY_PATH=/usr/local/lib

    # Shell prompt setting
    if [ -z $OS ] || [ $OS != "Windows_NT" ]; then
        export PS1='\[\033[32m\]\u@\h\[\033[34m\]:\w\[\033[00m\]\n\$ '
    fi

    # For subversion
    export SVN_EDITOR=emacsclient

    # set PATH so it includes user's private bin if it exists
    if [ -d $HOME/bin ] ; then
        PATH=$HOME/bin:$PATH
    fi

    # For Haskell
    if [ -d $HOME/.cabal/bin ]; then
        export PATH=$HOME/.cabal/bin:$PATH
    fi

    # For Ruby
    if [ -f /usr/bin/ruby ]; then
        RUBY_VERSION=`ruby --version | sed 's/^ruby\s\([0-9]\.[0-9]\).*$/\1/'`
        export PATH=$HOME/.gem/ruby/$RUBY_VERSION.0/bin:$PATH
    fi

    # For node.js (nvm)
    if [ -d $HOME/.nvm ]; then
        export NVM_DIR=$HOME/.nvm
        if [ -s "$NVM_DIR/nvm.sh" ]; then
            source $NVM_DIR/nvm.sh
        fi
    fi

    # Google depot tools
    if [ -d $HOME/work/depot_tools ]; then
        PATH=$HOME/work/depot_tools:$PATH
    fi

    # For Android
    if [ -d $HOME/Android/Sdk ]; then
        export ANDROID_SDK_ROOT=$HOME/Android/Sdk
        export ANDROID_MAKE_CCACHE=ccache
        PATH=$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/platform-tools:$PATH
    fi

    export PATH

    # Source local definitions
    if [ -f $HOME/.bash_profile.local ]; then
        . $HOME/.bash_profile.local
    fi
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	    . "$HOME/.bashrc"
    fi
fi

if [ ! -z $OS ] && [ $OS = "Windows_NT" ]; then
    cd ~
fi
