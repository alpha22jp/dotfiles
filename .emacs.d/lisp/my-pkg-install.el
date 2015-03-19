;;; my-pkg-install.el --- install my favorite packages in a lump
;;;

;;; Code:

(setq my:pkg-install-list
      '(ag
        autopair
        auto-complete
        diff-hl
        elscreen
        exec-path-from-shell
        flymake
        fuzzy-format
        git-gutter
        git-gutter-fringe
        magit
        markdown-mode
        multiple-cursors
        mc-extras
        projectile
        quickrun
        rcodetools
        region-bindings-mode
        recentf-ext
        simplenote2
        smart-mode-line
        wgrep-ag
        ))

(setq my:pkg-install-list-23
      '(color-theme-sanityinc-solarized
        git-gutter+
        git-gutter-fringe+
        ))

(setq my:pkg-install-list-24
      '(flycheck
        git-gutter+
        git-gutter-fringe+
        helm
        helm-ag
        helm-cscope
        helm-descbinds
        helm-gtags
        solarized-theme
        ))

(defun my:pkg-install ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg my:pkg-install-list)
    (if (not (locate-library (symbol-name pkg))) (package-install pkg)))
  (dolist (pkg (if (>= emacs-major-version 24)
                   my:pkg-install-list-24 my:pkg-install-list-23))
    (if (not (locate-library (symbol-name pkg))) (package-install pkg))))

(provide 'my-pkg-install)

;;; my-pkg-install.el ends here
