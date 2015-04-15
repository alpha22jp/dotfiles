;;; my-package-list.el --- install my favorite packages
;;;

;;; Code:

(defvar my-package-list
      '(ag
        auto-complete
        exec-path-from-shell
        expand-region
        flymake
        js2-mode
        json-mode
        magit
        markdown-mode
        multiple-cursors
        mc-extras
        projectile
        region-bindings-mode
        recentf-ext
        smartparens
        simplenote2
        wgrep-ag
        yasnippet
        ))

(defvar my-package-list-23
      '(anything
        color-theme-sanityinc-solarized
        git-gutter+
        git-gutter-fringe+
        ))

(defvar my-package-list-24
      '(flycheck
        git-gutter-fringe
        helm
        helm-ag
        helm-cscope
        helm-descbinds
        helm-gtags
        helm-swoop
        quickrun
        solarized-theme
        smart-mode-line
        ))

(let (packages)
  (mapc (lambda (e) (unless (package-installed-p e) (push e packages)))
          (append my-package-list (if (>= emacs-major-version 24)
                                      my-package-list-24 my-package-list-23)))
  (when packages
    (package-refresh-contents)
    (mapc (lambda (e) (package-install e)) packages)))

(provide 'my-package-list)

;;; my-package-list.el ends here
