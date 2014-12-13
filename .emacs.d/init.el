;; .emacs.d/init.el for Emacs 23,24
;;   Author: alpha22jp <alpha22jp@gmail.com>
;;   Created: 2008/06/05

;; ABCEDFGHIJKLMNOPQRSTUVWXYZ
;; abcdefghijklmnopqrstuvwxyz
;; 0123456789
;;
;; (frame-parameter nil 'font) ;; 使用中のフォントを調べる

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpa"))

(when (locate-library "package")
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;; (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/lisp"))

(add-hook 'window-setup-hook
          (lambda ()
            (set-face-foreground 'default "#cfcfcf")
            (set-face-background 'default "#101010")))

(add-hook 'after-make-frame-functions
          (lambda (f)
            (with-selected-frame f
              (keyboard-translate ?\C-h ?\C-?))))

(setq default-frame-alist
      (append (list
               '(font . "Migu 2M-11")
               '(cursor-color . "red3")
               '(alpha . 80)
               '(width .  80)
               '(height . 40))
              default-frame-alist))

(setq kill-whole-line t) ;; 行頭で "C-k" すると改行を含む行全体を削除
(setq auto-save-default nil) ;; 自動セーブしない
(setq make-backup-files nil) ;; バックアップファイルを作成しない

;; proxy settings
;;
(when (locate-library "my-proxy")
  (require 'my-proxy))

;; c/c++ mode settings
;;
(setq c-default-style "stroustrup")
(which-function-mode t)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (flymake-mode t)
             (setq indent-tabs-mode nil)
             (setq tab-width 4)))
(add-hook 'c++-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)))

;; hexl mode settings
;;
(setq hexl-options "-hex -group-by-8-bits")

;; java mode settings
;;
(add-to-list 'auto-mode-alist '("\\.as$" . java-mode))

;; compilation settings
;;
(setq compile-command "LANG=C make")
(setq compilation-scroll-output t)

;; diff settings
;;
(when (<= emacs-major-version 23)
  (require 'diff-color nil t))

;; flymake
;;
(when (locate-library "flymake")
  (require 'flymake)
  (require 'flymake-fringe nil t)
  (when (locate-library "flymake-popup")
    (require 'flymake-popup))
  (defun flymake-get-make-cmdline (source base-dir)
    (list "make" (list "-s" "-C" base-dir "LANG=C"
                       (concat "CHK_SOURCES=" source)
                       "SYNTAX_CHECK_MODE=1" "check-syntax")))
  (setq flymake-gui-warnings-enabled nil))

;; auto-complete
;;
(when (locate-library "auto-complete")
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-auto-start 3))

;; fuzzy-format
;;
(when (locate-library "fuzzy-format")
  (require 'fuzzy-format)
  (setq fuzzy-format-default-indent-tabs-mode t)
  (global-fuzzy-format-mode t))

;; elscreen
;;
(when (locate-library "elscreen")
  (require 'elscreen)
  (setq elscreen-prefix-key "\C-o")
  (elscreen-start)
  (add-hook 'dired-mode-hook
            '(lambda ()
               (local-unset-key "\C-o")))
  (add-hook 'svn-status-mode-hook
            '(lambda ()
               (local-unset-key "\C-o"))))

;; powerline
;;
(when (locate-library "powerline")
  (require 'powerline)
  (powerline-default-theme))

;; uniquify
;;
(when (locate-library "uniquify")
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; igrep
;;
(when (locate-library "igrep")
  (require 'igrep)
  (setq igrep-program "lgrep")
  (setq igrep-options "-Au8")
  (setq igrep-regex-option nil)
  (setq igrep-find t)
  (setq igrep-read-multiple-files t))

;; gtags
;;
(when (locate-library "gtags")
  (require 'gtags)
  (setq gtags-path-style 'relative)
  (setq gtags-select-buffer-single nil)
  (setq gtags-ignore-case nil) ;; 検索時に大文字・小文字を区別する
  (setq gtags-prefix-key nil)
  (setq gtags-auto-update t)
  (setq gtags-suggested-key-mapping t)
  (setq gtags-auto-update t))

;; psvn
;;
(when (locate-library "psvn")
  (setq svn-status-prefix-key '[(hyper s)])
  (require 'psvn)
  (require 'vc-svn) ;; Emacs23におけるSVN管理ファイル判定問題の対応のため
  (setq svn-status-hide-unmodified t)
  (setq svn-status-hide-unknown t)
  (setq svn-status-svn-file-coding-system 'utf-8))

;; magit
;;
;; (add-hook 'git-commit-mode-hook
;;           '(lambda ()
;;              (set-buffer-file-coding-system 'utf-8)))
(when (locate-library "auto-complete")
  (add-to-list 'ac-modes 'git-commit-mode))
;(add-to-list 'process-coding-system-alist '("git" utf-8 . utf-8))

;; my-vc-status
;; VCバックエンドに応じたstatus関数を呼び出す
(defun my-vc-status ()
  (interactive)
  (require 'vc)
  (cond ((eq (vc-deduce-backend) 'SVN) (call-interactively 'svn-status))
        ((eq (vc-deduce-backend) 'Git) (call-interactively 'magit-status))
        (t (message "Buffer is not version controlled"))))

;; fringe
;;
(when (locate-library "git-gutter-fringe")
  (require 'git-gutter-fringe)
  (global-git-gutter-mode t))
(when (locate-library "git-gutter-fringe+")
  (require 'git-gutter-fringe+)
  (global-git-gutter+-mode t))

;; cscope
;;
(when (locate-library "xcscope")
  (require 'xcscope))

;; eshell mode settings
;;
(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map (kbd "\C-a") 'eshell-bol)))

;; markdown mode
;;
(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; simplenote
;;
(when (locate-library "simplenote")
  (require 'simplenote)
  (when (locate-library "my-simplenote")
    (require 'my-simplenote))
  (simplenote-setup))

;; input method
;;
(when (locate-library "mozc")
  (require 'mozc)
  (add-hook 'mozc-mode-hook
            '(lambda ()
               (define-key mozc-mode-map [henkan] 'toggle-input-method)))
  (when (locate-library "ac-mozc")
    (require 'ac-mozc)
    (define-key ac-mode-map [muhenkan] 'ac-complete-mozc)))

;; anything
;;
(when (locate-library "anything")
  (require 'anything-startup))

;; wgrep-ag
;;
(when (locate-library "ag")
  (require 'ag)
  (when (>= emacs-major-version 24)
    (setq ag-highlight-search t))
  (setq ag-reuse-buffers t))
(when (locate-library "wgrep-ag")
  (add-hook 'ag-mode-hook
            '(lambda ()
               (require 'wgrep-ag)
               (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
               (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
               (wgrep-ag-setup))))

;; coding system settings
;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8)

;; global key bindings
;;
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-M-h") 'help-for-help)
(define-key ctl-x-map (kbd "C-b") 'bs-show)
(define-key ctl-x-map (kbd "t") 'toggle-truncate-lines)
(define-key ctl-x-map (kbd "C-z") 'kill-emacs)
(global-set-key (kbd "M-o") 'other-frame)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-n") 'forward-list)
(global-set-key (kbd "M-p") 'backward-list)
(global-set-key (kbd "C-M-n") 'end-of-defun)
(global-set-key (kbd "C-M-p") 'beginning-of-defun)
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-j") 'bs-cycle-next)
(global-set-key (kbd "M-k") 'bs-cycle-previous)
(global-set-key (kbd "M-h") 'elscreen-previous)
(global-set-key (kbd "M-l") 'elscreen-next)
(global-set-key [M-left] 'elscreen-previous)
(global-set-key [M-right] 'elscreen-next)
(global-set-key (kbd "M-,") 'cscope-pop-mark)
(global-set-key (kbd "M-.") 'cscope-find-global-definition)
(global-set-key (kbd "C-M-,") 'gtags-pop-stack)
(global-set-key (kbd "C-M-.") 'gtags-find-tag)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c C-d") 'simplenote-push-buffer)
(global-set-key (kbd "C-c C-e") 'simplenote-pull-buffer)
(global-set-key (kbd "C-x b") 'anything-for-files)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(global-set-key (kbd "C-x M-x") 'anything-M-x)
(global-set-key [hiragana-katakana] 'dabbrev-expand)
(global-set-key [henkan] 'toggle-input-method)
(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'elscreen-previous)
(global-set-key [f3] 'elscreen-next)
(global-set-key [f4] 'split-window-horizontally)
(global-set-key [f5] 'cscope-find-global-definition)
(global-set-key [f6] 'cscope-find-this-symbol)
(define-key global-map [(control f5)] 'gtags-find-tag)
(define-key global-map [(control f6)] 'gtags-find-rtag)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'ag)
(global-set-key [f9] 'svn-status-show-svn-log)
(global-set-key [f10] 'my-vc-status)
(global-set-key [f11] 'vc-diff)
(global-set-key [f12] 'vc-revert)
(global-set-key [C-tab] 'other-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(default-input-method "japanese-mozc")
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
