;;; lisp/init-navigation.el --- Navigation -*- lexical-binding: t; -*-

(use-package avy
  :bind
  (("C-'" . avy-goto-char-2)
   ("M-g g" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.2))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("M-y" . consult-yank-pop)
   ("M-g f" . consult-flymake)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-s f" . consult-find)
   ("M-s d" . consult-fd)
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s M-g" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s M-l" . consult-line-multi)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s M-l" . consult-line-multi)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  :custom
  (consult-buffer-sources
   '(consult-source-buffer
     consult-source-hidden-buffer
     consult-source-modified-buffer
     consult-source-other-buffer
     consult-source-buffer-register
     consult-source-file-register)))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)))

(provide 'init-navigation)

;;; lisp/init-navigation.el ends here
