;;; my-package-list.el --- install my favorite packages
;;;

;;; Commentary:

;;; Code:

(require 'package nil 'noerror)

(defvar my-package-list
  '(atomic-chrome
    ag
    cmake-mode
    company
    company-irony
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
    multiple-cursors
    mc-extras
    powerline
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

(unless (eq system-type 'windows-nt)
  (setq my-package-list
        (append my-package-list '(exec-path-from-shell mozc))))

(let (packages)
  (mapc (lambda (e) (unless (package-installed-p e) (push e packages)))
        my-package-list)
  (when packages
    (package-refresh-contents)
    (mapc (lambda (e) (package-install e)) packages)))

(provide 'my-package-list)

;;; my-package-list.el ends here
