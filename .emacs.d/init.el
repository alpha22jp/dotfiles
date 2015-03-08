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

;; package settings
;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))
(when (require 'package nil 'noerror)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;; (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

;; local elisp settings
;;
(defvar my:lisp-dir (expand-file-name "~/.emacs.d/lisp"))
(mapc (lambda (e) (if (and e (file-directory-p e)) (add-to-list 'load-path e)))
      (directory-files my:lisp-dir t "^[^.]"))
(add-to-list 'load-path my:lisp-dir)

(require 'my-pkg-install nil 'noerror)

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

;; general settings
;;
(tool-bar-mode 0) ;; ツールバーを表示しない
(tooltip-mode 0) ;; ツールチップを表示しない
(set-scroll-bar-mode 'right) ;; スクロールバーを右側に表示
(column-number-mode) ;; モードラインに桁数を表示する
(show-paren-mode) ;; 対応する括弧を強調表示する
(fset 'yes-or-no-p 'y-or-n-p) ;; "yes/no" が必要なときも "y/n" だけにする
(setq kill-whole-line t) ;; 行頭で "C-k" すると改行を含む行全体を削除
(setq auto-save-default nil) ;; 自動セーブしない
(setq make-backup-files nil) ;; バックアップファイルを作成しない
(setq inhibit-startup-screen t) ;; スタートアップ画面を表示しない
(setq read-file-name-completion-ignore-case t) ;; ファイル名補完でignore case
(setq find-file-visit-truename t) ;; シンボリックリンクを実体のパスで開く

;; exec-path-from-shell
;;
(when (require 'exec-path-from-shell nil 'noerror)
  (exec-path-from-shell-initialize))

;; color-theme
;;
(if (locate-library "color-theme-sanityinc-solarized")
    (if (>= emacs-major-version 24)
        (load-theme 'sanityinc-solarized-dark t)
      (when (require 'color-theme nil 'noerror)
        (require 'color-theme-sanityinc-solarized nil 'noerror)
        (color-theme-sanityinc-solarized-dark)))
  ;; No theme found, use manual settings
  (add-hook 'window-setup-hook
            (lambda ()
              (set-face-foreground 'default "#cfcfcf")
              (set-face-background 'default "#101010"))))

;; proxy settings
;;
(require 'my-proxy nil 'noerror)

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
(add-hook 'swift-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (setq indent-tabs-mode nil)))

;; ruby mode settings
;;
(when (require 'rcodetools nil 'noerror)
  (add-hook 'ruby-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'xmp))))

;; compilation settings
;;
(setq compile-command "LANG=C make")
(setq compilation-scroll-output t)

;; diff settings
;;
(require 'diff-color nil 'noerror)

;; autoinsert
;;
(when (require 'autoinsert nil 'noerror)
  (auto-insert-mode)
  (setq auto-insert-alist
        '((("\\.\\(c\\|cpp\\)$" . "C/C++ source") . ["template.c" my:template-insert-template])
          (("\\.\\(h\\|hpp\\)$" . "C/C++ header") . ["template.h" my:template-insert-template])))
  (setq auto-insert-directory "~/.emacs.d/template/")
  (setq auto-insert-query t)
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
(when (require 'autopair nil 'noerror)
  (autopair-global-mode))

;; flymake
;;
(when (require 'flymake nil 'noerror)
  (require 'flymake-fringe nil 'noerror)
  (when (locate-library "popup") (require 'flymake-popup nil 'noerror))
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
  (setq flycheck-display-errors-delay 0.5))

;; auto-complete
;;
(when (require 'auto-complete-config nil 'noerror)
  (ac-config-default)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t))

;; fuzzy-format
;;
(when (require 'fuzzy-format nil 'noerror)
  (setq fuzzy-format-default-indent-tabs-mode nil)
  (global-fuzzy-format-mode t))

;; elscreen
;;
(when (require 'elscreen nil 'noerror)
  (setq elscreen-prefix-key "\C-o")
  (elscreen-start)
  (add-hook 'dired-mode-hook
            (lambda () (local-unset-key "\C-o")))
  (add-hook 'svn-status-mode-hook
            (lambda () (local-unset-key "\C-o"))))

;; uniquify
;;
(when (require 'uniquify nil 'noerror)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; igrep
;;
(when (require 'igrep nil 'noerror)
  (setq igrep-program "lgrep")
  (setq igrep-options "-Au8")
  (setq igrep-regex-option nil)
  (setq igrep-find t)
  (setq igrep-read-multiple-files t))

;; gtags
;;
(when (require 'gtags nil 'noerror)
  (setq gtags-path-style 'relative)
  (setq gtags-select-buffer-single nil)
  (setq gtags-ignore-case nil) ;; 検索時に大文字・小文字を区別する
  (setq gtags-prefix-key nil)
  (setq gtags-auto-update t)
  (setq gtags-suggested-key-mapping t)
  (setq gtags-auto-update t))

;; psvn
;;
(when (require 'psvn nil 'noerror)
;  (setq svn-status-prefix-key '[(hyper s)])
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
(when (require 'git-gutter+)
  (global-git-gutter+-mode t)
  (require 'git-gutter-fringe+ nil 'noerror)
  (define-key git-gutter+-mode-map (kbd "M-n") 'git-gutter+-next-hunk)
  (define-key git-gutter+-mode-map (kbd "M-p") 'git-gutter+-previous-hunk)
  (define-key git-gutter+-mode-map (kbd "M-l") 'git-gutter+-show-hunk)
  (define-key git-gutter+-mode-map (kbd "M-r") 'git-gutter+-revert-hunk))

;; diff-hl
;;
(when (require 'diff-hl nil 'noerror)
  (global-diff-hl-mode)
  (define-key diff-hl-mode-map (kbd "M-n") 'diff-hl-next-hunk)
  (define-key diff-hl-mode-map (kbd "M-p") 'diff-hl-previous-hunk)
  (define-key diff-hl-mode-map (kbd "M-l") 'diff-hl-diff-goto-hunk)
  (define-key diff-hl-mode-map (kbd "M-r") 'diff-hl-revert-hunk)
  ;; git-gutter+が使えるときはdiff-hlはオフにする
  (add-hook 'find-file-hook
            (lambda ()
              (when (and (locate-library "git-gutter+") (git-gutter+-mode))
                (diff-hl-mode 0)))))

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
(require 'xcscope nil 'noerror)

;; eshell mode settings
;;
(add-hook 'eshell-mode-hook
          (lambda ()
	    (define-key eshell-mode-map (kbd "\C-a") 'eshell-bol)))

;; markdown mode
;;
(add-to-list 'auto-mode-alist
             '("\\.md\\'\\|app\\.simplenote\\.com_" . markdown-mode))

;; simplenote2
;;
(when (require 'simplenote2 nil 'noerror)
  (require 'my-simplenote2 nil 'noerror)
  (simplenote2-setup))

;; anything
;;
(when (require 'anything-startup nil 'noerror)
  (when (>= emacs-major-version 24)
    ;; 補完バッファのヘッダのフェイスを変更 (選択行と重複して見にくいので)
    (set-face-attribute 'anything-header nil
                        :inherit nil :underline t :weight 'bold))
  ;; バッファ補完候補の除外設定に "flymake:" を追加
  (setq anything-c-boring-buffer-regexp
        "\\(\\` \\)\\|\\*anything\\|\\*ac-mode\\| \\*Echo Area\\| \\*Minibuf\\|flymake:"))

;; helm
;;
(when (require 'helm-config nil 'noerror)
  (setq helm-delete-minibuffer-contents-from-point t)
  (setq helm-buffer-max-length 35)
  ;; バッファの並び順を変更しない
  (defadvice helm-buffers-sort-transformer (around ignore activate)
    (setq ad-return-value (ad-get-arg 0)))
  (eval-after-load "helm-files"
    '(progn
       (define-key helm-find-files-map (kbd "C-i") 'helm-execute-persistent-action))))

;; helm-gtags
;;
(when (require 'helm-gtags nil 'noerror)
  (add-hook 'c-mode-hook (lambda () (helm-gtags-mode)))
  ;; customize
  (setq helm-c-gtags-path-style 'relative)
  (setq helm-c-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-pulse-at-cursor nil)
  ;; key bindings
  (add-hook 'helm-gtags-mode-hook
            '(lambda ()
               (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
               (local-set-key (kbd "M-@") 'helm-gtags-find-rtag)
               (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
               (local-set-key (kbd "M-,") 'helm-gtags-pop-stack))))

;; helm-ag
;;
(setq helm-ag-insert-at-point 'symbol)

;; wgrep-ag
;;
(when (require 'ag nil 'noerror)
  (when (>= emacs-major-version 24)
    (setq ag-highlight-search t))
  (setq ag-reuse-buffers t))
(when (require 'wgrep-ag nil 'noerror)
  (add-hook 'ag-mode-hook
            (lambda ()
              (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
              (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
              (wgrep-ag-setup))))

;; recentf
;;
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("/.simplenote2/*" "/TAGS$" "/COMMIT_EDITMSG$"))
(require 'recentf-ext nil 'noerror)

;; multiple-cursors
;;
(when (require 'multiple-cursors nil 'noerror)
  (when (locate-library "autopair")
    (add-to-list 'mc/unsupported-minor-modes 'autopair-mode))
  (require 'mc-extras nil 'noerror))

;; region bindings mode
;;
(when (require 'region-bindings-mode nil 'noerror)
  (region-bindings-mode-enable)
  (define-key region-bindings-mode-map (kbd "<tab>") 'indent-region)
  (define-key region-bindings-mode-map (kbd "C-t") 'mc/mark-all-like-this-dwim)
  (define-key region-bindings-mode-map (kbd "C-l") 'mc/edit-lines)
  (define-key region-bindings-mode-map (kbd "M-p") 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map (kbd "M-n") 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map (kbd "M-u") 'mc/remove-current-cursor)
  (define-key region-bindings-mode-map (kbd "C-M-n") 'mc/cycle-forward)
  (define-key region-bindings-mode-map (kbd "C-M-p") 'mc/cycle-backward))

;; projectile
;;
(when (locate-library "projectile")
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c C-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c C-r") 'projectile-recentf)
  (define-key projectile-mode-map (kbd "C-c C-s") 'helm-projectile-ag))

;; coding system settings
;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8)

;; input method
;;
(when (require 'mozc nil 'noerror)
  (setq default-input-method "japanese-mozc")
  (define-key mozc-mode-map [henkan] 'toggle-input-method)
  (when (require 'ac-mozc nil 'noerror)
    (define-key ac-mode-map [muhenkan] 'ac-complete-mozc)))

;; global key bindings
;;
(global-set-key (kbd "M-h") 'help-for-help)
(define-key ctl-x-map (kbd "b") 'helm-for-files)
(define-key ctl-x-map (kbd "C-b") 'bs-show)
(define-key ctl-x-map (kbd "C-d") 'helm-descbinds)
(define-key ctl-x-map (kbd "t") 'toggle-truncate-lines)
(define-key ctl-x-map (kbd "C-z") 'kill-emacs)
(define-key ctl-x-map (kbd "x") 'helm-M-x)
(global-set-key (kbd "M-o") 'other-frame)
(global-set-key (kbd "C-t") 'mc/mark-all-dwim)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-:") 'helm-mini)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "C-M-n") 'end-of-defun)
(global-set-key (kbd "C-M-p") 'beginning-of-defun)
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
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
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
(global-set-key [f8] 'helm-ag)
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
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline nil))))
 '(flymake-warnline ((t (:underline nil))))
 '(helm-selection ((t (:inherit highlight :background "firebrick4")))))

(provide 'init)

;;; init.el ends here
