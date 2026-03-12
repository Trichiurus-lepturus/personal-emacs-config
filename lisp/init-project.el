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
  :ensure nil
  :defer t
  :init
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" user-emacs-directory))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave/" user-emacs-directory))
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp
                     (expand-file-name "tramp-backups/"
                                       user-emacs-directory)))
  (setq vc-handled-backends nil)
  (setq tramp-verbose 1)
  (setq tramp-chunksize 65536)
  (setq tramp-use-connection-share t)
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=~/.ssh/tramp-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=10m"))
  (setq enable-remote-dir-locals t)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process))

(use-package tramp-hlo
  :after tramp
  :config
  (tramp-hlo-setup))

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable \
--group-directories-first --no-group"))

(use-package dirvish
  :defer t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq delete-by-moving-to-trash t)
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse
                   git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  (setq dirvish-large-directory-threshold 16384)
  (setq dirvish-default-layout nil)
  (setq dirvish-preview-dispatchers '())
  (setq dirvish-subtree-state-style 'nerd)
  (setq dirvish-path-separators
        (list
         (format "  %s " (nerd-icons-codicon "nf-cod-home"))
         (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
         (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq mouse-1-click-follows-link nil)
  (define-key dirvish-mode-map (kbd "<mouse-1>")
              'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-3>")
              'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<double-mouse-1>")
              'dired-mouse-find-file)
  :bind
  (("C-c d d" . dirvish-dwim)
   ("C-c d s" . dirvish-side)
   ("C-c d f" . dirvish-fd)
   :map dirvish-mode-map
   (";"   . dired-up-directory)
   ("h"   . dirvish-dispatch)
   ("a"   . dirvish-setup-menu)
   ("f"   . dirvish-file-info-menu)
   ("o"   . dirvish-quick-access)
   ("s"   . dirvish-quicksort)
   ("r"   . dirvish-history-jump)
   ("l"   . dirvish-ls-switches-menu)
   ("v"   . dirvish-vc-menu)
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)
   ("M-t" . dirvish-layout-toggle)))

(provide 'init-project)

;;; lisp/init-project.el ends here
