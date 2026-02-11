;;; lisp/init-fundamental.el --- Fundamental -*- lexical-binding: t; -*-

(require 'init-const)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(when is-windows
  (set-clipboard-coding-system 'utf-16le-dos)
  (setq process-coding-system-alist
        '((".*" . utf-8-unix))))

(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq auto-save-default t
      auto-save-interval 1200
      auto-save-timeout 120)

(provide 'init-fundamental)

;;; lisp/init-fundamental.el ends here
