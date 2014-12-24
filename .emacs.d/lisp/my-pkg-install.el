;;; my-pkg-install.el --- install my favorite packages in a lump
;;;

;;; Code:

(setq my:pkg-install-list
  '(ag
    anything
    autopair
    auto-complete
    color-theme-sanityinc-solarized
    elscreen
    fuzzy-format
    git-gutter+
    git-gutter-fringe+
    iedit
    magit
    markdown-mode
    region-bindings-mode
    recentf-ext
    simplenote
    wgrep-ag
    xcscope))

(defun my:pkg-install ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg my:pkg-install-list)
    (if (not (locate-library (symbol-name pkg))) (package-install pkg))))

(provide 'my-pkg-install)

;;; my-pkg-install.el ends here
