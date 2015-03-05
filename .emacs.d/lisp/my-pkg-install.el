;;; my-pkg-install.el --- install my favorite packages in a lump
;;;

;;; Code:

(setq my:pkg-install-list
  '(ag
    autopair
    auto-complete
    color-theme-sanityinc-solarized
    diff-hl
    elscreen
    exec-path-from-shell
    flymake
    flycheck
    fuzzy-format
    git-gutter+
    git-gutter-fringe+
    helm
    helm-descbinds
    helm-gtags
    magit
    markdown-mode
    multiple-cursors
    mc-extras
    rcodetools
    region-bindings-mode
    recentf-ext
    simplenote2
    wgrep-ag
    xcscope))

(defun my:pkg-install ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg my:pkg-install-list)
    (if (not (locate-library (symbol-name pkg))) (package-install pkg))))

(provide 'my-pkg-install)

;;; my-pkg-install.el ends here
