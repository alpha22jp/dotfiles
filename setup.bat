@echo off
cd /d %~dp0
cd ..

if exist .gitconfig (
   rename .gitconfig .gitconfig.bak
)
mklink .gitconfig dotfiles\.gitconfig

if not exist .emacs.d (
   mkdir .emacs.d
)

cd .emacs.d
if exist init.el (
   rename init.el init.el.bak
)
mklink init.el ..\dotfiles\.emacs.d\init.el

if exist lisp (
   rename lisp lisp.bak
)
mklink /D lisp ..\dotfiles\.emacs.d\lisp

if exist lisp-local (
   rename lisp-local lisp-local.bak
)
mkdir lisp-local > NUL 2>&1
