;;; my-pkg-install.el --- install my favorite packages in a lump
;;;

;;; Code:

(package-refresh-contents)

(package-install 'anything)
(package-install 'autopair)
(package-install 'auto-complete)
(package-install 'fuzzy-format)
(package-install 'elscreen)
(package-install 'xcscope)
(package-install 'git-gutter-fringe)
(package-install 'magit)
(package-install 'ag)
(package-install 'wgrep-ag)
(package-install 'markdown-mode)
(package-install 'simplenote)
(package-install 'color-theme-sanityinc-solarized)

(message "Install completed.")

(provide 'my-pkg-install)

;;; my-pkg-install.el ends here
