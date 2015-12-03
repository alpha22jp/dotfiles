;;; my-package-list.el --- install my favorite packages
;;;

;;; Code:

(defvar my-package-list
  '(ag
    auto-complete
    auto-complete-clang-async
    exec-path-from-shell
    expand-region
    flycheck
    flymake
    google-c-style
    git-gutter-fringe
    haskell-mode
    helm
    helm-ag
    helm-c-yasnippet
    helm-cscope
    helm-descbinds
    helm-gtags
    helm-swoop
    js2-mode
    json-mode
    markdown-mode
    multiple-cursors
    mc-extras
    quickrun
    region-bindings-mode
    recentf-ext
    smartparens
    simplenote2
    smart-mode-line
    solarized-theme
    wgrep-ag
    yasnippet
    ))

(defvar my-package-list-24.4
  '(magit
    ))

(let (packages)
  (mapc (lambda (e) (unless (package-installed-p e) (push e packages)))
          (append my-package-list (if (>= emacs-minor-version 4)
                                      my-package-list-24.4 nil)))
  (when packages
    (package-refresh-contents)
    (mapc (lambda (e) (package-install e)) packages)))

(provide 'my-package-list)

;;; my-package-list.el ends here
