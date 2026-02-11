;;; lisp/init-project.el --- Project -*- lexical-binding: t; -*-

(use-package project
  :ensure nil
  :bind
  (("C-x p f" . project-find-file)
   ("C-x p b" . project-switch-to-buffer)
   ("C-x p r" . project-query-replace-regexp))
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" "f")
          (project-find-regexp "Ripgrep" "g")
          (project-switch-to-buffer "Buffer" "b")
          (magit-project-status "Magit" "m"))))

(use-package envrc
  :commands (envrc-mode envrc-global-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package tramp
  :defer t
  :init
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" user-emacs-directory))
  :config
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave/" user-emacs-directory))
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  (setq vc-handled-backends nil)
  (setq tramp-verbose 1)
  (setq tramp-chunksize 2048)
  (setq tramp-use-connection-share nil)
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=~/.ssh/tramp-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=10m")))

(provide 'init-project)

;;; lisp/init-project.el ends here
