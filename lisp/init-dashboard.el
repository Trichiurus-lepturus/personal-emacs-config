;;; lisp/init-dashboard.el --- Dashboard -*- lexical-binding: t -*-

(require 'cl-lib)

(use-package recentf
  :ensure nil
  :init (recentf-mode 1))

(defvar-local sztk-dashboard--was-visible nil)

(use-package dashboard
  :defer t
  :init
  (setq dashboard-projects-backend 'project-el)
  (add-hook 'dashboard-before-initialize-hook #'recentf-mode)

  (defun sztk-dashboard-choose-banner ()
    (let* ((banner-dir (expand-file-name "banners/"
                                         user-emacs-directory))
           (width (window-width))
           (has-png (and (display-graphic-p)
                         (image-type-available-p 'png)))
           (banner-file (cond
                         (has-png "emacs-china.png")
                         ((>= width 80) "ascii-art.txt")
                         (t "alt-ascii-art.txt"))))
      (setq dashboard-startup-banner
            (expand-file-name banner-file banner-dir))))
  (add-hook 'dashboard-before-initialize-hook
            #'sztk-dashboard-choose-banner)

  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-center-content t
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
