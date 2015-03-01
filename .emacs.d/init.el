;;; .emacs.d/init.el --- init.el for Emacs 23,24
;;   Author: alpha22jp <alpha22jp@gmail.com>
;;   Created: 2008/06/05

;;; Commentary:

;; ABCEDFGHIJKLMNOPQRSTUVWXYZ
;; abcdefghijklmnopqrstuvwxyz
;; 0123456789
;;
;; (frame-parameter nil 'font) ;; 使用中のフォントを調べる

;;; Code:

;; os-type
;;
(defvar my:os-type
  (if (string-match "apple-darwin" system-configuration) 'mac 'linux))

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpa"))

(when (locate-library "package")
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;; (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/lisp"))

(when (locate-library "my-pkg-install") (require 'my-pkg-install))

;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (set-face-foreground 'default "#cfcfcf")
;;             (set-face-background 'default "#101010")))

(setq default-frame-alist
      (append (list
               '(cursor-color . "red3")
               '(alpha . 90)
               '(width .  80)
               '(height . 40))
              default-frame-alist))

;; font setting
;;
(add-to-list
 'default-frame-alist
 (cons 'font (if (eq my:os-type 'mac) "Ricty Diminished-16" "Migu 2M-11")))

;; keyboard-translate settings
;;
(defun my:keyboard-translate ()
  (when (eq my:os-type 'mac) (keyboard-translate ?¥ ?\\))
  (keyboard-translate ?\C-h ?\C-?))

(add-hook 'after-make-frame-functions
          (lambda (f)
            (with-selected-frame f (my:keyboard-translate))))
(my:keyboard-translate)

(fset 'yes-or-no-p 'y-or-n-p) ;; "yes/no" が必要なときも "y/n" だけにする
(setq kill-whole-line t) ;; 行頭で "C-k" すると改行を含む行全体を削除
(setq auto-save-default nil) ;; 自動セーブしない
(setq make-backup-files nil) ;; バックアップファイルを作成しない
(setq inhibit-startup-screen t) ;; スタートアップ画面を表示しない
(setq read-file-name-completion-ignore-case t) ;; ファイル名補完でignore case
(setq find-file-visit-truename t) ;; シンボリックリンクを実体のパスで開く

;; exec-path-from-shell
;;
(when (locate-library "exec-path-from-shell")
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; color-theme
;;
(when (locate-library "color-theme-sanityinc-solarized")
  (if (>= emacs-major-version 24)
      (load-theme 'sanityinc-solarized-dark t)
    (when (require 'color-theme nil t)
      (require 'color-theme-sanityinc-solarized)
      (color-theme-sanityinc-solarized-dark))))

;; proxy settings
;;
(when (locate-library "my-proxy")
  (require 'my-proxy))

;; c/c++ mode settings
;;
(setq c-default-style "stroustrup")
(which-function-mode t)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (flymake-mode t)
            (c-toggle-hungry-state 1)
	    (local-unset-key (kbd "C-M-h"))
	    (setq comment-column 4)
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)))
(add-hook 'c++-mode-hook
          (lambda ()
	    (setq c-basic-offset 4)))

;; hexl mode settings
;;
(setq hexl-options "-hex -group-by-8-bits")

;; java mode settings
;;
(add-to-list 'auto-mode-alist '("\\.as$" . java-mode))

;; swift mode settings
;;
(when (locate-library "swift-mode")
  (add-hook 'swift-mode-hook
            (lambda ()
              (auto-complete-mode t)
              (setq indent-tabs-mode nil))))

;; ruby mode settings
;;
(when (locate-library "rcodetools")
  (require 'rcodetools)
  (add-hook 'ruby-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'xmp))))

;; compilation settings
;;
(setq compile-command "LANG=C make")
(setq compilation-scroll-output t)

;; diff settings
;;
(when (locate-library "diff-color")
  (require 'diff-color nil t))

;; autoinsert
;;
(when (locate-library "autoinsert")
  (require 'autoinsert)
  (auto-insert-mode)
  (defun my:template-get-filename-base ()
    (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
  (defun my:template-get-filename-ext ()
    (file-name-extension (file-name-nondirectory (buffer-file-name))))
  (defvar my:template-replacement-alist
    '(("%file%"          . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%file-base%"     . (lambda () (my:template-get-filename-base)))
      ("%date%"          . (lambda () (format-time-string "%Y-%m-%d")))
      ("%include-guard%" . (lambda () (format "__%s_%s__"
                                              (upcase (my:template-get-filename-base))
                                              (upcase (my:template-get-filename-ext)))))))
  (defun my:template-insert-template ()
    (mapc (lambda (c)
	    (progn
	      (replace-string (car c) (funcall (cdr c)) nil)
	      (goto-char (point-min))))
          my:template-replacement-alist)))

;; autopair
;;
(when (locate-library "autopair")
  (require 'autopair)
  (autopair-global-mode))

;; flymake
;;
(when (locate-library "flymake")
  (require 'flymake)
  (require 'flymake-fringe nil t)
  (when (and (locate-library "popup")
	     (locate-library "flymake-popup"))
    (require 'flymake-popup))
  (defun flymake-get-make-cmdline (source base-dir)
    (list "make" (list "-s" "-C" base-dir "LANG=C"
                       (concat "CHK_SOURCES=" source)
                       "SYNTAX_CHECK_MODE=1" "check-syntax")))
  (setq flymake-gui-warnings-enabled nil))

;; flycheck
;;
(when (locate-library "flycheck")
  ;; エラーをポップアップで表示
  (setq flycheck-display-errors-function
        (lambda (errors)
          (let ((messages (mapcar #'flycheck-error-message errors)))
            (popup-tip (mapconcat 'identity messages "\n")))))
  (setq flycheck-display-errors-delay 0.5)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; auto-complete
;;
(when (locate-library "auto-complete")
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t))

;; fuzzy-format
;;
(when (locate-library "fuzzy-format")
  (require 'fuzzy-format)
  (setq fuzzy-format-default-indent-tabs-mode nil)
  (global-fuzzy-format-mode t))

;; elscreen
;;
(when (locate-library "elscreen")
  (require 'elscreen)
  (setq elscreen-prefix-key "\C-o")
  (elscreen-start)
  (add-hook 'dired-mode-hook
            (lambda () (local-unset-key "\C-o")))
  (add-hook 'svn-status-mode-hook
            (lambda () (local-unset-key "\C-o"))))

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
(when (locate-library "auto-complete")
  (add-to-list 'ac-modes 'git-commit-mode))
(when (not (functionp 'process-live-p))
  (defun process-live-p (process)
    "Returns non-nil if PROCESS is alive"
    (memq (process-status process)
	  '(run open listen connect stop))))

;; git-gutter+
;;
(when (locate-library "git-gutter+")
  (require 'git-gutter+)
  (global-git-gutter+-mode t)
  (when (locate-library "git-gutter-fringe+")
    (require 'git-gutter-fringe+))
  (define-key git-gutter+-mode-map (kbd "M-n") 'git-gutter+-next-hunk)
  (define-key git-gutter+-mode-map (kbd "M-p") 'git-gutter+-previous-hunk)
  (define-key git-gutter+-mode-map (kbd "M-l") 'git-gutter+-show-hunk)
  (define-key git-gutter+-mode-map (kbd "M-r") 'git-gutter+-revert-hunk))

;; my-vc-status
;; VCバックエンドに応じたstatus関数を呼び出す
(defun my-vc-status ()
  "Call VC status function depending on backend."
  (interactive)
  (require 'vc)
  (cond ((eq (vc-deduce-backend) 'SVN) (call-interactively 'svn-status))
        ((eq (vc-deduce-backend) 'Git) (call-interactively 'magit-status))
        (t (message "Buffer is not version controlled"))))

;; cscope
;;
(when (locate-library "xcscope")
  (require 'xcscope))

;; eshell mode settings
;;
(add-hook 'eshell-mode-hook
          (lambda ()
	    (define-key eshell-mode-map (kbd "\C-a") 'eshell-bol)))

;; markdown mode
;;
(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
               '("\\.md\\'\\|app\\.simplenote\\.com_" . markdown-mode)))

;; simplenote2
;;
(when (locate-library "simplenote2")
  (require 'simplenote2)
  (when (locate-library "my-simplenote2")
    (require 'my-simplenote2))
  (simplenote2-setup))

;; anything
;;
(when (locate-library "anything")
  (require 'anything-startup)
  (when (>= emacs-major-version 24)
    ;; 補完バッファのヘッダのフェイスを変更 (選択行と重複して見にくいので)
    (set-face-attribute 'anything-header nil
                        :inherit nil :underline t :weight 'bold))
  ;; バッファ補完候補の除外設定に "flymake:" を追加
  (setq anything-c-boring-buffer-regexp
        "\\(\\` \\)\\|\\*anything\\|\\*ac-mode\\| \\*Echo Area\\| \\*Minibuf\\|flymake:"))

;; wgrep-ag
;;
(when (locate-library "ag")
  (require 'ag)
  (when (>= emacs-major-version 24)
    (setq ag-highlight-search t))
  (setq ag-reuse-buffers t))
(when (locate-library "wgrep-ag")
  (add-hook 'ag-mode-hook
            (lambda ()
	      (require 'wgrep-ag)
	      (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
	      (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
	      (wgrep-ag-setup))))

;; recentf
;;
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("/.simplenote2/*" "/TAGS$" "/COMMIT_EDITMSG$"))
(when (locate-library "recentf-ext") (require 'recentf-ext))

;; multiple-cursors
;;
(when (locate-library "multiple-cursors")
  (require 'multiple-cursors)
  (when (locate-library "autopair")
    (add-to-list 'mc/unsupported-minor-modes 'autopair-mode))
  (when (locate-library "mc-extras") (require 'mc-extras)))

;; region bindings mode
;;
(when (locate-library "region-bindings-mode")
  (require 'region-bindings-mode)
  (region-bindings-mode-enable)
  (define-key region-bindings-mode-map (kbd "<tab>") 'indent-region)
  (define-key region-bindings-mode-map (kbd "C-t") 'mc/mark-all-like-this-dwim)
  (define-key region-bindings-mode-map (kbd "M-p") 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map (kbd "M-n") 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map (kbd "M-u") 'mc/remove-current-cursor)
  (define-key region-bindings-mode-map (kbd "C-M-n") 'mc/cycle-forward)
  (define-key region-bindings-mode-map (kbd "C-M-p") 'mc/cycle-backward))

;; coding system settings
;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8)

;; input method
;;
(when (locate-library "mozc")
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (define-key mozc-mode-map [henkan] 'toggle-input-method)
  (when (locate-library "ac-mozc")
    (require 'ac-mozc)
    (define-key ac-mode-map [muhenkan] 'ac-complete-mozc)))

;; global key bindings
;;
(global-set-key (kbd "C-M-h") 'help-for-help)
(define-key ctl-x-map (kbd "C-b") 'bs-show)
(define-key ctl-x-map (kbd "t") 'toggle-truncate-lines)
(define-key ctl-x-map (kbd "C-z") 'kill-emacs)
(global-set-key (kbd "M-o") 'other-frame)
(global-set-key (kbd "C-t") 'mc/mark-all-dwim)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "C-M-n") 'end-of-defun)
(global-set-key (kbd "C-M-p") 'beginning-of-defun)
;(global-set-key (kbd "M-j") 'bs-cycle-next)
;(global-set-key (kbd "M-k") 'bs-cycle-previous)
;(global-set-key (kbd "M-h") 'elscreen-previous)
;(global-set-key (kbd "M-l") 'elscreen-next)
(global-set-key [M-left] 'elscreen-previous)
(global-set-key [M-right] 'elscreen-next)
(global-set-key (kbd "M-,") 'cscope-pop-mark)
(global-set-key (kbd "M-.") 'cscope-find-global-definition)
(global-set-key (kbd "C-M-,") 'gtags-pop-stack)
(global-set-key (kbd "C-M-.") 'gtags-find-tag)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c C-d") 'simplenote2-push-buffer)
(global-set-key (kbd "C-c C-e") 'simplenote2-pull-buffer)
(global-set-key (kbd "C-x b") 'anything-for-files)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(global-set-key (kbd "C-x M-x") 'anything-M-x)
(global-set-key [hiragana-katakana] 'dabbrev-expand)
(global-set-key [henkan] 'toggle-input-method)
(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'elscreen-previous)
(global-set-key [f3] 'elscreen-next)
(global-set-key [f4] 'split-window-vertically)
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
 '(auto-insert-alist (quote ((("\\.\\(c\\|cpp\\)$" . "C/C++ source") . ["template.c" my:template-insert-template]) (("\\.\\(h\\|hpp\\)$" . "C/C++ header") . ["template.h" my:template-insert-template]))))
 '(auto-insert-directory "~/.emacs.d/template/")
 '(auto-insert-query t)
 '(column-number-mode t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline nil))))
 '(flymake-warnline ((t (:underline nil)))))

(provide 'init)

;;; init.el ends here
