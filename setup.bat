@echo off
cd /d %~dp0
cd ..

if exist .gitconfig (
   rename .gitconfig .gitconfig.bak
)
mklink .gitconfig dotfiles\.gitconfig

if exist .bashrc (
   rename .bashrc .bashrc.bak
)
mklink .bashrc dotfiles\.bashrc

if exist .bash_profile (
   rename .bash_profile .bash_profile.bak
)
mklink .bash_profile dotfiles\.bash_profile
