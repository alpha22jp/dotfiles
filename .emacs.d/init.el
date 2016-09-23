;;; .emacs.d/init.el --- init.el for Emacs24
;;   Author: alpha22jp <alpha22jp@gmail.com>
;;   Created: 2008/06/05

;;; Commentary:

;;; Code:

;; package settings
;;
(when (require 'package nil 'noerror)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "http://melpa.org/packages/")))
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
(defvar my-default-font-family
  (if (eq system-type 'darwin) "Ricty Diminished" "Migu 2M"))
(defvar my-default-font-size (if (eq system-type 'darwin) '16 '11))
(defun my-font-setting ()
  "My customized font setting function."
  (set-face-attribute 'default nil :font
                      (concat my-default-font-family "-"
                              (number-to-string my-default-font-size)))
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Ricty Diminished" :size 16)))
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
               '(cursor-color . "firebrick4")
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
(which-function-mode) ;; カーソル位置の関数名表示を有効にする
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
(setq compile-command "LANG=C make") ;; デフォルトのコンパイルコマンド
(setq compilation-scroll-output 't) ;; compilationバッファを出力に合わせてスクロール

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
(when (require 'uniquify nil 'noerror)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; use-package
;;
(require 'use-package nil 'noerror)
(unless (locate-library "use-package")
  (defmacro use-package (&rest args)))

;; exec-path-from-shell
;;
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; recentf
;;
(eval-after-load "recentf"
  '(progn
     (setq recentf-save-file "~/.emacs.d/.recentf")
     (setq recentf-max-saved-items 100)
     (setq recentf-exclude '("/.emacs.d/elpa/*" "/.simplenote2/*" "/TAGS$" "/COMMIT_EDITMSG$"))
     (use-package recentf-ext)))

;; autoinsert
;;
(use-package autoinsert
  :config
  (auto-insert-mode)
  (setq auto-insert-directory "~/.emacs.d/template/")
  (setq auto-insert-query t)
  (require 'my-auto-insert nil 'noerror))

;; yasnippet
;;
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;; companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
  (define-key yas-keymap (kbd "<tab>") nil)
  (yas-global-mode))

;; smartparens
;;
(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode))

;; company
;;
(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :bind ("C-M-i" . company-complete)
  :config
  ;; (setq company-idle-delay nil) ; 自動補完をしない
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("<tab>" . company-complete-selection))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

;; irony
;;
(use-package irony
  :diminish irony-mode
  :config
  (setq irony-additional-clang-options '("-std=c++11"))
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c-mode-common-hook 'irony-mode))

;; flycheck
;;
(use-package flycheck
  :config
  (setq flycheck-display-errors-function
        ;; エラーをポップアップで表示
        (lambda (errors)
          (let ((messages (mapcar #'flycheck-error-message errors)))
            (popup-tip (mapconcat 'identity messages "\n")))))
  (setq flycheck-display-errors-delay 0.5)
  (bind-keys :map flycheck-mode-map
             ("C-M-n" . flycheck-next-error)
             ("C-M-p" . flycheck-previous-error)))

;; flycheck-irony
;;
(use-package flycheck-irony
  :if (locate-library "flycheck")
  :config (flycheck-irony-setup))

;; flycheck-cpplint
;;
(use-package flycheck-google-cpplint
  :if (locate-library "flycheck")
  :config
  (setq flycheck-googlelint-extensions "cpp,hpp,c,h")
  (setq flycheck-googlelint-verbose "3")
  (setq flycheck-googlelint-linelength "120")
  (flycheck-add-next-checker 'irony '(warning . c/c++-googlelint)))

;; rtags
;;
(use-package rtags
  :config
  (setq rtags-use-helm t)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (rtags-is-indexed)
                (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
                (local-set-key (kbd "M-[") 'rtags-find-symbol)
                (local-set-key (kbd "M-@") 'rtags-find-references)
                (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))

;; fuzzy-format
;;
(use-package fuzzy-format
  :config
  (delq 'makefile-mode fuzzy-format-check-modes)
  (global-fuzzy-format-mode))

;; elscreen
;;
(use-package elscreen
  :config
  (setq elscreen-prefix-key "\C-o")
  (elscreen-start)
  (add-hook 'dired-mode-hook (lambda () (local-unset-key "\C-o")))
  (add-hook 'compilation-mode-hook (lambda () (local-unset-key "\C-o")))
  (add-hook 'svn-status-mode-hook (lambda () (local-unset-key "\C-o"))))

;; psvn
;;
(use-package psvn
  :config
  (setq svn-status-hide-unmodified t)
  (setq svn-status-hide-unknown t)
  (setq svn-status-svn-file-coding-system 'utf-8))

;; magit
;;
(use-package magit
  :defer t
  :config
  (setq magit-diff-refine-hunk t)
  (add-hook 'magit-mode-hook 'diff-mode-setup-faces))

;; git-gutter
;;
(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (setq git-gutter:handled-backends '(git svn))
  (global-git-gutter-mode t)
  (use-package git-gutter-fringe)
  (add-hook 'git-gutter-mode-hook
            (lambda ()
              (local-set-key (kbd "M-n") 'git-gutter:next-hunk)
              (local-set-key (kbd "M-p") 'git-gutter:previous-hunk)
              (local-set-key (kbd "M-l") 'git-gutter:popup-hunk)
              (local-set-key (kbd "M-r") 'git-gutter:revert-hunk))))

;; simplenote2
;;
(use-package simplenote2
  :disabled t
  :config
  (setq simplenote2-email user-mail-address)
  (setq simplenote2-markdown-notes-mode 'markdown-mode)
  (add-hook 'simplenote2-create-note-hook 'simplenote2-set-markdown)
  (add-hook 'simplenote2-note-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-t") 'simplenote2-add-tag)
              (local-set-key (kbd "C-c C-c") 'simplenote2-push-buffer)
              (local-set-key (kbd "C-c C-d") 'simplenote2-pull-buffer)))
  (require 'my-simplenote2 nil 'noerror)
  (simplenote2-setup))

;; ag
;;
(use-package ag
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

;; wgrep-ag
;;
(use-package wgrep-ag
  :defer t
  :config
  (setq wgrep-auto-save-buffer t) ;; 編集完了と同時に保存
  (setq wgrep-enable-key "r")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

;; helm
;;
(use-package helm
  :diminish helm-mode
  :config
  (setq helm-delete-minibuffer-contents-from-point t)
  (setq helm-buffer-max-length 35)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  ;; バッファの並び順を変更しない
  (defadvice helm-buffers-sort-transformer (around ignore activate)
    (setq ad-return-value (ad-get-arg 0)))
  (bind-key "C-o" 'helm-occur-from-isearch isearch-mode-map)
  (bind-keys ("C-;" . helm-mini)
             ("C-z" . helm-resume)
             ("M-y" . helm-show-kill-ring))
  (bind-keys :map ctl-x-map
             ("m" . helm-man-woman)
             ("x" . helm-M-x)
             ("C-a" . helm-apropos)
             ("C-b" . helm-buffers-list)
             ("C-d" . helm-descbinds)
             ("C-f" . helm-find-files)
             ("C-r" . helm-recentf)))
(use-package helm-files
  :config
  ;; ファイルが存在しないは何もしない
  (defadvice helm-ff-kill-or-find-buffer-fname (around ignore activate)
    (when (file-exists-p candidate)
      ad-do-it))
  (bind-key "<tab>" 'helm-execute-persistent-action helm-find-files-map))

;; helm-ag
;;
(use-package helm-ag
  :defer t
  :init (bind-key "C-g" 'helm-ag ctl-x-map)
  :config (setq helm-ag-insert-at-point 'symbol))

;; helm-swoop
;;
(use-package helm-swoop
  :defer t
  :bind ("C-t" . helm-swoop)
  :config (bind-key "C-t" 'helm-swoop-from-isearch isearch-mode-map))

;; helm-gtags
;;
(use-package helm-gtags
  :diminish helm-gtags-mode
  :config
  ;; (setq helm-c-gtags-path-style 'relative)
  ;; (setq helm-c-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-update-interval-second 0)
  (setq helm-gtags-pulse-at-cursor nil)
  (add-hook 'helm-gtags-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'helm-gtags-dwim)
              (local-set-key (kbd "M-@") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-[") 'helm-gtags-find-symbol)
              (local-set-key (kbd "M-,") 'helm-gtags-pop-stack))))

;; helm-cscope
;;
(use-package helm-cscope
  :disabled t
  :diminish helm-cscope-mode
  :config
  (require 'my-cscope nil 'noerror)
  (bind-keys :map helm-cscope-mode-map
             ("M-." . helm-cscope-find-global-definition)
             ("M-@" . helm-cscope-find-calling-this-funtcion)
             ("M-[" . helm-cscope-find-this-symbol)
             ("M-," . helm-cscope-pop-mark))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (locate-dominating-file default-directory "cscope.out")
                (helm-cscope-mode)))))

;; helm-c-yasnippet
;;
(use-package helm-c-yasnippet
  :if (locate-library "yasnippet")
  :init (bind-key "C-y" 'helm-yas-complete ctl-x-map)
  :config (setq helm-yas-space-match-any-greedy t))

;; multiple-cursors
;;
(use-package multiple-cursors
  :bind ("C-]" . mc/mark-all-dwim)
  :config (use-package mc-extras))

;; expand-region
;;
(use-package expand-region
  :bind
  ("C-@" . er/expand-region)
  ("C-M-@" . er/contract-region))

;; region bindings mode
;;
(use-package region-bindings-mode
  :diminish region-bindings-mode
  :config
  (region-bindings-mode-enable)
  (bind-key "<tab>" 'indent-region region-bindings-mode-map)
  (when (require 'multiple-cursors nil 'noerror)
    (bind-keys :map region-bindings-mode-map
               ("C-;" . comment-dwim)
               ("C-]" . mc/mark-all-like-this-dwim)
               ("C-l" . mc/edit-lines)
               ("M-n" . mc/mark-next-like-this)
               ("M-p" . mc/mark-previous-like-this)
               ("M-u" . mc/remove-current-cursor)
               ("C-M-n" . mc/cycle-forward)
               ("C-M-p" . mc/cycle-backward))))

;; smart-mode-line
;;
(use-package smart-mode-line
  :config
  (setq rm-blacklist "\\` Abbrev\\'\\|\\` MRev\\'")
  (setq sml/name-width 32)
  (add-to-list 'rm-text-properties '("\\` mc" 'face 'font-lock-warning-face))
  (sml/setup))

;; quickrun
;;
(use-package quickrun
  :defer t
  :config
  (quickrun-add-command
   "c++/g++"
   '((:exec . ("%c -std=c++11 -pthread -Wall -Werror -Weffc++ %o -o %e %s" "%e %a")))
   :override t)
  (quickrun-add-command
   "javascript/node-harmony"
   '((:command . "node")
     (:description . "Run Javascript file with node.js(harmony)")
     (:cmdopt . "--harmony")))
  (quickrun-set-default "javascript" "javascript/node-harmony"))

;; c/c++ mode
;;
(eval-after-load "cc-mode"
  '(progn
     (add-hook 'c-mode-common-hook
               (lambda ()
                 (c-set-style "stroustrup")
                 (setq c-basic-offset 4)
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
     (setq hexl-bits 8)
     (add-hook 'hexl-mode-hook
               (lambda ()
                 (local-set-key (kbd "C-c C-s") 'hexl-insert-hex-string)))))

;; javascript mode
;;
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(eval-after-load "js2-mode"
  (setq js-indent-level 2))

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

;; input method
;;
(use-package mozc
  :bind ("<henkan>" . toggle-input-method)
  :config
  (setq default-input-method "japanese-mozc")
  (bind-key "<henkan>" 'toggle-input-method mozc-mode-map))

;; global key bindings
;;
(bind-keys :map ctl-x-map
           ("C-z" . save-buffers-kill-emacs)
           ("t" . toggle-truncate-lines))
(bind-keys ("M-h" . help-for-help)
           ("C-M-h" . help-for-help)
           ("M-g" . goto-line)
           ("C-^" . delete-indentation)
           ("M-^" . next-error)
           ("C-M-^" . previous-error)
           ("C-," . beginning-of-buffer)
           ("C-." . end-of-buffer)
           ("M-(" . backward-list)
           ("M-)" . forward-list)
           ("C-<tab>" . other-window)
           ("<f1>" . delete-other-windows)
           ("<f2>" . elscreen-previous)
           ("<f3>" . elscreen-next)
           ("<f4>" . split-window-vertically)
           ("<f5>" . quickrun)
           ("<f6>" . describe-personal-keybindings)
           ("<f7>" . compile)
           ("<f8>" . ag)
           ("<f9>" . vc-print-log)
           ("<f10>" . my-vc-status)
           ("<f11>" . vc-diff)
           ("<f12>" . vc-revert)
           ("<hiragana-katakana>" . dabbrev-expand))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (wgrep-ag use-package swift-mode solarized-theme smartparens smart-mode-line simplenote2 rtags region-bindings-mode recentf-ext quickrun popup mozc mc-extras markdown-mode magit json-mode js2-mode helm-swoop helm-gtags helm-descbinds helm-cscope helm-c-yasnippet helm-ag google-c-style git-gutter-fringe ghc flycheck-irony flycheck-haskell expand-region exec-path-from-shell company-irony cmake-mode ag haskell-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:inherit highlight :background "firebrick4")))))

(provide 'init)

;;; init.el ends here
