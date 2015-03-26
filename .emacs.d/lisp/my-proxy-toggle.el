;;; my-proxy-toggle.el --- Toggle URL proxy settings
;;

;;; Code:

(defvar my-proxy-use-proxy t)

(defvar my-proxy-setting
  (list (cons "no_proxy" (getenv "no_proxy"))
        (cons "http" (getenv "http_proxy"))
        (cons "https" (getenv "https_proxy"))))

(defun my-proxy-toggle-proxy ()
  (interactive)
  (if my-proxy-use-proxy
      (progn
        (setq url-proxy-services nil)
        (setq my-proxy-use-proxy nil)
        (message "Unset proxy services"))
    (setq url-proxy-services my-proxy-setting)
    (setq my-proxy-use-proxy t)
    (message "Set proxy services")))

(when my-proxy-use-proxy
  (setq url-proxy-services my-proxy-setting))

(provide 'my-proxy-toggle)

;;; my-proxy-toggle.el ends here
