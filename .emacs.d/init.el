;;; .emacs.d/init.el --- init.el for Emacs24
;;   Author: alpha22jp <alpha22jp@gmail.com>
;;   Created: 2008/06/05

;;; Commentary:

;;; Code:

;; package settings
;;
(when (require 'package nil 'noerror)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize))

;; local lisp path settings
;;
(defvar my-lisp-dir-list '("~/.emacs.d/lisp" "~/.emacs.d/lisp-local"))
(mapc (lambda (dir)
        (mapc (lambda (e) (if (file-directory-p e)
                              (add-to-list 'load-path (expand-file-name e))))
              (directory-files dir t "^[^.]"))
        (add-to-list 'load-path (expand-file-name dir)))
      my-lisp-dir-list)

;; font settings
;;
(defvar my-default-font "Ricty Diminished")
(defun my-font-setting ()
  "My customized font setting function."
  (set-face-attribute 'default nil :family my-default-font :height 120)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family my-default-font :size 16)))
(unless (string= (frame-parameter nil 'font) "tty") (my-font-setting))

;; keyboard-translate settings
;;
(defun my-keyboard-translate ()
  "My customized keyboard translation function."
  (when (eq system-type 'darwin) (keyboard-translate ?¥ ?\\))
  (keyboard-translate ?\C-h ?\C-?))
(my-keyboard-translate)

;; frame settings
;;
(setq default-frame-alist
      (append (list
               '(cursor-color . "red3")
               '(alpha . 90)
               '(width .  80)
               '(height . 40))
              default-frame-alist))
(add-hook 'after-make-frame-functions
          (lambda (f)
            (with-selected-frame f
              (my-font-setting)
              (my-keyboard-translate))))

;; personal information settings
;;
(setq user-full-name "alpha22jp")
(setq user-mail-address "alpha22jp@gmail.com")

;; general settings
;;
(tool-bar-mode 0) ;; ツールバーを表示しない
(tooltip-mode 0) ;; ツールチップを表示しない
(menu-bar-mode 0) ;; メニューバーを表示しない
(set-scroll-bar-mode 'right) ;; スクロールバーを右側に表示
(column-number-mode) ;; モードラインに桁数を表示する
(show-paren-mode) ;; 対応する括弧を強調表示する
(fset 'yes-or-no-p 'y-or-n-p) ;; "yes/no" が必要なときも "y/n" だけにする
(setq kill-whole-line t) ;; 行頭で "C-k" すると改行を含む行全体を削除
(setq auto-save-default nil) ;; 自動セーブしない
(setq create-lockfiles nil) ;; ロックファイルを作成しない
(setq make-backup-files nil) ;; バックアップファイルを作成しない
(setq inhibit-startup-screen t) ;; スタートアップ画面を表示しない
(setq read-file-name-completion-ignore-case t) ;; ファイル名補完でignore case
(setq find-file-visit-truename t) ;; シンボリックリンクを実体のパスで開く
(setq-default tab-width 4 indent-tabs-mode nil) ;; インデント幅は4で空白を使用
(setq ad-redefinition-action 'accept) ;; defadviceによる二重定義の警告を無視

;; exec-path-from-shell
;;
(when (require 'exec-path-from-shell nil 'noerror)
  (exec-path-from-shell-initialize))

;; extra local settings
;;
(require 'my-package-list nil 'noerror) ;; パッケージの一括インストール
(require 'my-proxy-toggle nil 'noerror) ;; HTTPプロキシをトグルする
(require 'diff-color nil 'noerror) ;; diffのカラー表示設定
(require 'isearch-region nil 'noerror) ;; リージョンをisearchできるようにする
(require 'my-vc-status nil 'noerror) ;; VCバックエンドに応じたstatus関数を呼び出す

;; color-theme
;;
(if (locate-library "solarized-theme")
    (load-theme 'solarized-dark t)
  (add-hook 'window-setup-hook
            (lambda ()
              (set-face-foreground 'default "#cfcfcf")
              (set-face-background 'default "#101010"))))

;; uniquify
;;
(require 'uniquify nil 'noerror)
(custom-set-variables
    '(uniquify-buffer-name-style 'post-forward-angle-brackets))

;; which-function-mode
;;
(which-function-mode)
(custom-set-variables
 '(which-func-modes '(c-mode c++-mode java-mode ruby-mode python-mode)))

;; compilation settings
;;
(custom-set-variables
 '(compile-command "LANG=C make")
 '(compilation-scroll-output t))

;; autoinsert
;;
(when (require 'autoinsert nil 'noerror)
  (auto-insert-mode)
  (custom-set-variables
   '(auto-insert-directory "~/.emacs.d/template/")
   '(auto-insert-query t))
  (require 'my-auto-insert nil 'noerror))

;; yasnippet
;;
(eval-after-load "yasnippet"
  '(progn
     ;; companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
     (define-key yas-keymap (kbd "<tab>") nil)
     (yas-global-mode 1)))

;; smartparens
;;
(when (require 'smartparens-config nil 'noerror)
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode))

;; company-mode
;;
(when (locate-library "company")
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  ;; (setq company-idle-delay nil) ; 自動補完をしない
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; irony-mode
;;
(eval-after-load "irony"
  '(progn
     (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
     (add-to-list 'company-backends 'company-irony)
     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
     (add-hook 'c-mode-common-hook 'irony-mode)))

;; flycheck
;;
(when (require 'flycheck nil 'noerror)
  (custom-set-variables
   ;; エラーをポップアップで表示
   '(flycheck-display-errors-function
     (lambda (errors)
       (let ((messages (mapcar #'flycheck-error-message errors)))
         (popup-tip (mapconcat 'identity messages "\n")))))
   '(flycheck-display-errors-delay 0.5))
  (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
  (add-hook 'c-mode-common-hook 'flycheck-mode))

;; flycheck-irony
;;
(eval-after-load "flycheck"
  '(when (locate-library "flycheck-irony") (flycheck-irony-setup)))

;; flycheck-cpplint
;;
(eval-after-load "flycheck"
  '(progn
     (when (require 'flycheck-google-cpplint nil 'noerror)
       (custom-set-variables
           '(flycheck-googlelint-extensions "cpp,hpp,c,h")
           '(flycheck-googlelint-verbose "3")
           '(flycheck-googlelint-linelength "120"))
       (when (locate-library "flycheck-irony")
         (flycheck-add-next-checker 'irony '(warning . c/c++-googlelint))))))

;; rtags
;;
(when (require 'rtags nil 'noerror)
  (custom-set-variables '(rtags-use-helm t))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (rtags-is-indexed)
                (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
                (local-set-key (kbd "M-;") 'rtags-find-symbol)
                (local-set-key (kbd "M-@") 'rtags-find-references)
                (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))

;; fuzzy-format
;;
(when (require 'fuzzy-format nil 'noerror)
  (delq 'makefile-mode fuzzy-format-check-modes)
  (global-fuzzy-format-mode t))

;; elscreen
;;
(when (require 'elscreen nil 'noerror)
  (custom-set-variables '(elscreen-prefix-key "\C-o"))
  (elscreen-start)
  (add-hook 'dired-mode-hook
            (lambda () (local-unset-key "\C-o")))
  (add-hook 'svn-status-mode-hook
            (lambda () (local-unset-key "\C-o"))))

;; psvn
;;
(when (require 'psvn nil 'noerror)
  (custom-set-variables
   '(svn-status-hide-unmodified t)
   '(svn-status-hide-unknown t)
   '(svn-status-svn-file-coding-system 'utf-8)))

;; magit
;;
(eval-after-load "magit"
  '(progn
     (add-hook 'magit-mode-hook 'diff-mode-setup-faces)
     (custom-set-variables '(magit-diff-refine-hunk 't))))

;; git-gutter
;;
(when (locate-library "git-gutter")
  (global-git-gutter-mode t)
  (custom-set-variables '(git-gutter:handled-backends '(git svn)))
  (require 'git-gutter-fringe nil 'noerror)
  (add-hook 'git-gutter-mode-hook
            (lambda ()
              (local-set-key (kbd "M-n") 'git-gutter:next-hunk)
              (local-set-key (kbd "M-p") 'git-gutter:previous-hunk)
              (local-set-key (kbd "M-l") 'git-gutter:popup-hunk)
              (local-set-key (kbd "M-r") 'git-gutter:revert-hunk))))

;; simplenote2
;;
(eval-after-load "simplenote2"
  '(progn
     (custom-set-variables
      '(simplenote2-email "alpha22jp@gmail.com")
      '(simplenote2-markdown-notes-mode 'markdown-mode))
     (add-hook 'simplenote2-create-note-hook 'simplenote2-set-markdown)
     (add-hook 'simplenote2-note-mode-hook
               (lambda ()
                 (local-set-key (kbd "C-c C-t") 'simplenote2-add-tag)
                 (local-set-key (kbd "C-c C-c") 'simplenote2-push-buffer)
                 (local-set-key (kbd "C-c C-d") 'simplenote2-pull-buffer)))
     (require 'my-simplenote2 nil 'noerror)
     (simplenote2-setup)))

;; helm
;;
(when (require 'helm-config nil 'noerror)
  (custom-set-variables
   '(helm-delete-minibuffer-contents-from-point t)
   '(helm-buffer-max-length 35)
   '(helm-autoresize-min-height 20))
  (helm-autoresize-mode 1)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  (define-key isearch-mode-map (kbd "C-t") 'helm-swoop-from-isearch)
  ;; バッファの並び順を変更しない
  (defadvice helm-buffers-sort-transformer (around ignore activate)
    (setq ad-return-value (ad-get-arg 0))))
(eval-after-load "helm-files"
  '(define-key helm-find-files-map (kbd "C-i") 'helm-execute-persistent-action))

;; helm-gtags
;;
(eval-after-load "helm-gtags"
  '(progn
     (custom-set-variables
      ;; '(helm-c-gtags-path-style 'relative)
      ;; '(helm-c-gtags-ignore-case t)
      '(helm-gtags-auto-update t)
      '(helm-gtags-update-interval-second 0)
      '(helm-gtags-pulse-at-cursor nil))
     (add-hook 'helm-gtags-mode-hook
               (lambda ()
                 (local-set-key (kbd "M-.") 'helm-gtags-dwim)
                 (local-set-key (kbd "M-@") 'helm-gtags-find-rtag)
                 (local-set-key (kbd "M-;") 'helm-gtags-find-symbol)
                 (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)))))

;; helm-cscope
;;
(when (require 'helm-cscope nil 'noerror)
  (require 'my-cscope nil 'noerror)
  (define-key helm-cscope-mode-map (kbd "M-.") 'helm-cscope-find-global-definition)
  (define-key helm-cscope-mode-map (kbd "M-@") 'helm-cscope-find-calling-this-funtcion)
  (define-key helm-cscope-mode-map (kbd "M-;") 'helm-cscope-find-this-symbol)
  (define-key helm-cscope-mode-map (kbd "M-,") 'helm-cscope-pop-mark)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (locate-dominating-file default-directory "cscope.out")
                (helm-cscope-mode)))))

;; helm-c-yasnippet
;;
(when (require 'helm-c-yasnippet nil 'noerror)
  (custom-set-variables '(helm-yas-space-match-any-greedy t))
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

;; helm-ag
;;
(eval-after-load "helm-ag"
  '(custom-set-variables '(helm-ag-insert-at-point 'symbol)))

;; wgrep-ag
;;
(eval-after-load "ag"
  '(progn
     (custom-set-variables
      '(ag-highlight-search t)
      '(ag-reuse-buffers t))
     (require 'wgrep-ag nil 'noerror)))
(eval-after-load "wgrep-ag"
  '(progn
     (custom-set-variables
      '(wgrep-auto-save-buffer t) ; 編集完了と同時に保存
      '(wgrep-enable-key "r")) ; "r" キーで編集モードに
     (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

;; recentf
;;
(eval-after-load "recentf"
  '(progn
     (custom-set-variables
      '(recentf-save-file "~/.emacs.d/.recentf")
      '(recentf-max-saved-items 100)
      '(recentf-exclude '("/.simplenote2/*" "/TAGS$" "/COMMIT_EDITMSG$")))
     (require 'recentf-ext nil 'noerror)))

;; multiple-cursors
;;
(when (require 'multiple-cursors nil 'noerror)
  (require 'mc-extras nil 'noerror))

;; expand-region
;;
(require 'expand-region nil 'noerror)

;; region bindings mode
;;
(when (require 'region-bindings-mode nil 'noerror)
  (region-bindings-mode-enable)
  (define-key region-bindings-mode-map (kbd "<tab>") 'indent-region)
  (define-key region-bindings-mode-map (kbd "C-]") 'mc/mark-all-like-this-dwim)
  (define-key region-bindings-mode-map (kbd "C-l") 'mc/edit-lines)
  (define-key region-bindings-mode-map (kbd "M-p") 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map (kbd "M-n") 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map (kbd "M-u") 'mc/remove-current-cursor)
  (define-key region-bindings-mode-map (kbd "C-M-n") 'mc/cycle-forward)
  (define-key region-bindings-mode-map (kbd "C-M-p") 'mc/cycle-backward))

;; smart-mode-line
;;
(when (require 'smart-mode-line nil 'noerror)
  (custom-set-variables
   '(rm-blacklist
     "\\` Projectile\\|\\` Helm\\|\\` GitGutter\\'\\|\\` pair\\'\\|\
\\` Abbrev\\'\\|\\` MRev\\'\\|\\` rk\\'\\|\\` company\\'\\|\\` Irony\\'\\|\\` SP\\|\\` yas\\'")
   '(sml/name-width 32))
  (add-to-list 'rm-text-properties '("\\` mc" 'face 'font-lock-warning-face))
  (sml/setup))

;; quickrun
;;
(eval-after-load "quickrun"
  '(progn
     (quickrun-add-command
      "c++/g++"
      '((:exec . ("%c -std=c++11 -pthread -Wall -Werror -Weffc++ %o -o %e %s" "%e %a")))
      :override t)
     (quickrun-add-command
      "javascript/node-harmony"
      '((:command . "node")
        (:description . "Run Javascript file with node.js(harmony)")
        (:cmdopt . "--harmony")))
     (quickrun-set-default "javascript" "javascript/node-harmony")))

;; c/c++ mode
;;
(eval-after-load "cc-mode"
  '(progn
     (custom-set-variables '(c-default-style "stroustrup"))
     (add-hook 'c-mode-common-hook
               (lambda ()
                 (custom-set-variables '(c-basic-offset 4))
                 (c-set-offset 'case-label 0)
                 (c-set-offset 'member-init-intro '+)
                 (when (locate-library "helm-gtags")
                   (when (locate-dominating-file default-directory "GTAGS")
                     (helm-gtags-mode)))
                 (c-toggle-hungry-state 1)
                 (setq truncate-lines t)
                 (setq comment-column 4)))
     (add-hook 'c++-mode-hook
               (lambda ()
                 (c-set-offset 'access-label -3)))))

;; emacs-lisp mode
;;
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; hexl mode
;;
(eval-after-load "hexl"
  '(progn
     ;; (setq hexl-options "-hex -group-by-8-bits")
     (custom-set-variables '(hexl-bits 8))
     (add-hook 'hexl-mode-hook
               (lambda ()
                 (local-set-key (kbd "C-c C-s") 'hexl-insert-hex-string)))))

;; javascript mode
;;
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(eval-after-load "js2-mode"
  '(custom-set-variables '(js-indent-level 2)))

;; json mode
;;
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; ruby mode
;;
(eval-after-load "ruby-mode"
'(progn
   (require 'rcodetools nil 'noerror)
   (add-hook 'ruby-mode-hook
             (lambda ()
               (local-set-key (kbd "C-c C-c") 'xmp)))))

;; haskell mode
;;
(add-hook 'haskell-mode-hook
          (lambda ()
            ;; (setq haskell-process-show-debug-tips nil)
            (interactive-haskell-mode 1)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indentation)))

;; eshell mode
;;
(eval-after-load "esh-mode"
  '(progn
     (add-hook 'eshell-mode-hook
               (lambda ()
                 (define-key eshell-mode-map (kbd "\C-a") 'eshell-bol)))))

;; markdown mode
;;
(add-to-list 'auto-mode-alist
             '("\\.md\\'\\|app\\.simplenote\\.com_" . markdown-mode))

;; coding system settings
;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8)

;; input method
;;
(when (require 'mozc nil 'noerror)
  (custom-set-variables '(default-input-method "japanese-mozc"))
  (define-key mozc-mode-map [henkan] 'toggle-input-method))

;; global key bindings
;;
(global-set-key (kbd "M-h") 'help-for-help)
(define-key ctl-x-map (kbd "C-a") 'helm-apropos)
(define-key ctl-x-map (kbd "C-b") 'helm-buffers-list)
(define-key ctl-x-map (kbd "C-d") 'helm-descbinds)
(define-key ctl-x-map (kbd "C-g") 'helm-ag)
(define-key ctl-x-map (kbd "m") 'helm-man-woman)
(define-key ctl-x-map (kbd "C-r") 'helm-recentf)
(define-key ctl-x-map (kbd "C-z") 'save-buffers-kill-emacs)
(define-key ctl-x-map (kbd "C-f") 'helm-find-files)
(define-key ctl-x-map (kbd "t") 'toggle-truncate-lines)
(define-key ctl-x-map (kbd "x") 'helm-M-x)
(global-set-key (kbd "C-]") 'mc/mark-all-dwim)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-:") 'helm-mini)
(global-set-key (kbd "C-^") 'delete-indentation)
(global-set-key (kbd "C-t") 'helm-swoop)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)
(global-set-key (kbd "M-^") 'next-error)
(global-set-key (kbd "C-M-^") 'previous-error)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-(") 'backward-list)
(global-set-key (kbd "M-)") 'forward-list)
(global-set-key [M-left] 'elscreen-previous)
(global-set-key [M-right] 'elscreen-next)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-z") 'helm-resume)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key [hiragana-katakana] 'dabbrev-expand)
(global-set-key [henkan] 'toggle-input-method)
(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'elscreen-previous)
(global-set-key [f3] 'elscreen-next)
(global-set-key [f4] 'split-window-vertically)
(global-set-key [f5] 'quickrun)
(global-set-key [f6] 'simplenote2-browse)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'ag)
(global-set-key [f9] 'vc-print-log)
(global-set-key [f10] 'my-vc-status)
(global-set-key [f11] 'vc-diff)
(global-set-key [f12] 'vc-revert)
(global-set-key [C-tab] 'other-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:inherit highlight :background "firebrick4")))))

(provide 'init)

;;; init.el ends here
