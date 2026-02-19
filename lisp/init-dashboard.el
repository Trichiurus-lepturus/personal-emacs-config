;;; lisp/init-dashboard.el --- Dashboard -*- lexical-binding: t -*-

(use-package recentf
  :ensure nil
  :init (recentf-mode 1))

(defvar-local sztk-dashboard--was-visible nil)

(use-package dashboard
  :defer t
  :init
  (setq dashboard-projects-backend 'project-el)
  (add-hook 'dashboard-before-initialize-hook #'recentf-mode)
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner
        `(,(expand-file-name "banner/emacs-china.png" user-emacs-directory)
          . ,(expand-file-name "banner/ascii-art.txt" user-emacs-directory))
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-items '((projects . 6)
                          (recents  . 6))
        dashboard-footer-messages '("Esc - Meta - Alt - Ctrl - Shift"))

  (defun sztk-auto-kill-dashboard (&rest _)
    (if-let ((buf (get-buffer "*dashboard*")))
        (with-current-buffer buf
          (if-let ((is-visible (get-buffer-window buf 'visible)))
              (setq-local sztk-dashboard--was-visible t)
            (when sztk-dashboard--was-visible
              (remove-hook 'buffer-list-update-hook
                           #'sztk-auto-kill-dashboard)
              (run-with-idle-timer
               0 nil
               (lambda ()
                 (when (buffer-live-p buf) (kill-buffer buf)))))))
      (remove-hook 'buffer-list-update-hook #'sztk-auto-kill-dashboard)))

  (defun sztk-dashboard--do-register-cleanup-hook ()
    (when (and (fboundp 'daemonp) (daemonp))
      (remove-hook 'server-after-make-frame-hook
                   #'sztk-dashboard--do-register-cleanup-hook)
      (when-let ((buf (get-buffer "*dashboard*")))
        (switch-to-buffer buf)
        (with-current-buffer buf
          (dashboard-refresh-buffer))))
    (add-hook 'buffer-list-update-hook #'sztk-auto-kill-dashboard))

  (defun sztk-dashboard-register-cleanup-hook ()
    (if (and (fboundp 'daemonp) (daemonp))
        (add-hook 'server-after-make-frame-hook
                  #'sztk-dashboard--do-register-cleanup-hook)
      (sztk-dashboard--do-register-cleanup-hook)))

  (add-hook 'dashboard-after-initialize-hook
            #'sztk-dashboard-register-cleanup-hook))

(provide 'init-dashboard)

;;; lisp/init-dashboard.el ends here
