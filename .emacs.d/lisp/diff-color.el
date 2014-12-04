;;; diff-color.el --- diff��ɽ���򥫥�ե�ˤ��� (Emacs23����)
;;
;; ����: http://www.clear-code.com/blog/2012/4/3.html

;; diff��ɽ����ˡ���ѹ�
(defun diff-mode-setup-faces ()
  ;; �ɲä��줿�Ԥ��Ф�ɽ��
  (set-face-attribute 'diff-added nil :background "#335533")
  ;; ������줿�Ԥ��֤�ɽ��
  (set-face-attribute 'diff-removed nil :background "#553333")
  ;; ʸ��ñ�̤Ǥ��ѹ��ս�Ͽ���ȿž���ƶ�Ĵ
  (set-face-attribute 'diff-refine-change nil
                      :foreground nil :background nil
                      :weight 'bold :inverse-video t))
(add-hook 'diff-mode-hook 'diff-mode-setup-faces)

;; diff��ɽ�������餹����ʸ��ñ�̤Ǥζ�Ĵɽ����Ԥ�
(defun diff-mode-refine-automatically ()
  (diff-auto-refine-mode t))
(add-hook 'diff-mode-hook 'diff-mode-refine-automatically)

;; diff��Ϣ������
(defun magit-setup-diff ()
  ;; diff��ɽ�����Ƥ���Ȥ���ʸ��ñ�̤Ǥ��ѹ��ս�⶯Ĵɽ������
  ;; 'all�ǤϤʤ�t�ˤ���ȸ����������hunk�Τ߶�Ĵɽ������
  (setq magit-diff-refine-hunk 'all)
  ;; diff�Ѥ�face�����ꤹ��
  (diff-mode-setup-faces)
  ;; diff��ɽ�����꤬��񤭤���Ƥ��ޤ��Τǥϥ��饤�Ȥ�̵���ˤ���
  (set-face-attribute 'magit-item-highlight nil :inherit nil))
(add-hook 'magit-mode-hook 'magit-setup-diff)

(provide 'diff-color)