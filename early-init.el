;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

(setq-default frame-background-mode 'dark)
(setq frame-inhibit-implied-resize t)
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(font . "Fira Mono-12"))
(unless (daemonp)
  (add-to-list 'default-frame-alist '(visibility . nil))
  (add-hook 'window-setup-hook (lambda () (make-frame-visible))))
(setq package-enable-at-startup nil)

;;; early-init.el ends here
