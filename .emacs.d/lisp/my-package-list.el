;;; my-package-list.el --- install my favorite packages
;;;

;;; Commentary:

;;; Code:

(require 'package nil 'noerror)

(defvar my-package-list
  '(ag
    cmake-mode
    company
    company-irony
    exec-path-from-shell
    expand-region
    flycheck
    flycheck-irony
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
    irony
    js2-mode
    json-mode
    magit
    markdown-mode
    mozc
    multiple-cursors
    mc-extras
    quickrun
    region-bindings-mode
    recentf-ext
    rtags
    smartparens
    simplenote2
    smart-mode-line
    solarized-theme
    swift-mode
    use-package
    wgrep-ag
    yasnippet
    ))

(let (packages)
  (mapc (lambda (e) (unless (package-installed-p e) (push e packages)))
        my-package-list)
  (when packages
    (package-refresh-contents)
    (mapc (lambda (e) (package-install e)) packages)))

(provide 'my-package-list)

;;; my-package-list.el ends here
