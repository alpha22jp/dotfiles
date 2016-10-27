;; my-powerline.el --- My powerline theme
;;

;;; Commentary:
;; Based on: http://blechmusik.hatenablog.jp/entry/2013/12/13/020823

;;; Code:

(unless (functionp 'ime-get-mode)
  (defun ime-get-mode () current-input-method))

(defun get-buffer-file-eol-type ()
  (case (coding-system-eol-type buffer-file-coding-system)
    (0 "LF")
    (1 "CRLF")
    (2 "CR")
    (otherwise "??")))

(defun get-buffer-coding-type-without-eol-type ()
  (cl-labels
      ((remove-os-info (string)
                       (replace-regexp-in-string "-\\(dos\\|unix\\|mac\\)$" "" string)))
    (lexical-let
        ((string
          (replace-regexp-in-string "-with-signature" "(bom)"
                                    (remove-os-info  (symbol-name buffer-file-coding-system)))))
      (if (string-match-p "(bom)" string)
          (downcase string)
        (upcase string)))))

(defface powerline-active3
  '((t (:background "Springgreen4" :inherit mode-line-inactive
        :foreground "white")))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-inactive3
  '((t (:background "grey60" :inherit mode-line-inactive)))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-active4
  '((t (:background "VioletRed" :inherit mode-line-inactive
        :foreground "white")))
  "Powerline face 4."
  :group 'powerline)

(defface powerline-inactive4
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 4."
  :group 'powerline)

(defface powerline-active5
  '((t (:background "Yellow4" :inherit mode-line-inactive
        :foreground "white")))
  "Powerline face 5."
  :group 'powerline)

(defface powerline-inactive5
  '((t (:background "grey60" :inherit mode-line-inactive)))
  "Powerline face 5."
  :group 'powerline)

(defface powerline-active6
  '((t (:background "grey80" :inherit mode-line-inactive)))
  "Powerline face 6."
  :group 'powerline)

(defface powerline-inactive6
  '((t (:background "grey60" :inherit mode-line-inactive)))
  "Powerline face 6."
  :group 'powerline)

(defpowerline powerline-ime-mode
  (if (ime-get-mode) "[あ]" "[Aa]"))

(defpowerline powerline-coding-type
   (concat (get-buffer-coding-type-without-eol-type) "[" (get-buffer-file-eol-type) "]"))

(defpowerline powerline-buffer-status
  (concat (if (buffer-modified-p) "M" "-") ":"
          (if buffer-read-only "R" "-")))

(defun powerline-my-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face3 (if active 'powerline-active3 'powerline-inactive3))
                          (face4 (if active 'powerline-active4 'powerline-inactive4))
                          (face5 (if active 'powerline-active5 'powerline-inactive5))
                          (face6 (if active 'powerline-active6 'powerline-inactive6))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list
                                (powerline-ime-mode (if (ime-get-mode) face4 face3) 'l)
                                (funcall separator-left (if (ime-get-mode) face4 face3) nil)
                                (powerline-coding-type nil 'l)
                                (powerline-buffer-status nil 'l)
                                (funcall separator-left face2 face6)
                                (powerline-buffer-id face6 'l)
                                (funcall separator-left face6 face1)
                                (powerline-major-mode face1 'l)
                                (powerline-narrow face1 'l)
                                (powerline-minor-modes face1 'l)))
                          (center (list (powerline-raw " " face1)
                                        (funcall separator-left face1 face2)
                                        (powerline-process face2)
                                        (powerline-vc face2)))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                          (powerline-raw which-func-format face2 'r))
                                     (funcall separator-right face2 face5)
                                     (powerline-raw "%4l" face5 'r)
                                     (powerline-raw ":" face5)
                                     (powerline-raw "%3c" face5 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(provide 'my-powerline)

;;; my-powerline.el ends here
