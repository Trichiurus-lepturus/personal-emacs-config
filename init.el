;;; init.el --- Main Emacs Config -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq package-enable-at-startup nil)

;; System
(defconst is-windows (eq system-type 'windows-nt))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(when is-windows
  (set-clipboard-coding-system 'utf-16le-dos)
  (setq process-coding-system-alist
        '((".*" . utf-8-unix))))

(defun sztk-path-msys2-to-windows (path)
  (let ((p (expand-file-name path)))
    (if (executable-find "cygpath")
        (string-trim (shell-command-to-string
                      (format "cygpath -w \"%s\"" p)))
      (convert-standard-filename p))))

;; GC
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Backup
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

;; Package
(require 'package)

(when is-windows
  (setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

;; UI
(setq-default indent-tabs-mode nil
              fill-column 76
              require-final-newline t)

(setq default-input-method nil
      winner-mode 1
      repeat-mode 1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      use-dialog-box nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(global-font-lock-mode 1)

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Scroll
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)

(defun +pixel-scroll-interpolate-down (&optional arg)
  (interactive "P")
  (if pixel-scroll-precision-interpolate-page
      (pixel-scroll-precision-interpolate
       (if arg
           (* -1 (prefix-numeric-value arg) (pixel-line-height))
         (- (* 0.75 (window-text-height nil t)))) nil 1)
    (condition-case nil
	(scroll-up arg)
      (end-of-buffer (goto-char (point-max))))))

(defun +pixel-scroll-interpolate-up (&optional arg)
  (interactive "P")
  (if pixel-scroll-precision-interpolate-page
      (pixel-scroll-precision-interpolate
       (if arg
           (* (prefix-numeric-value arg) (pixel-line-height))
         (* 0.75 (window-text-height nil t))) nil 1)
    (condition-case nil
	(scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

;; Fonts
(use-package faces
  :ensure nil
  :preface
  (defun sztk-setup-fonts (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (set-face-attribute 'default nil :family "Fira Mono" :height 120)
      (set-face-attribute 'fixed-pitch nil :family "Fira Mono")
      (set-fontset-font t 'han (font-spec :family "Noto Sans CJK SC"))
      (set-fontset-font t 'kana (font-spec :family "Noto Sans CJK JP"))
      (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR"))
      (set-fontset-font t 'bopomofo
                        (font-spec :family "Noto Sans CJK TC"))))
  :config
  (add-to-list 'default-frame-alist '(font . "Fira Mono"))
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'sztk-setup-fonts)
    (sztk-setup-fonts)))

;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Scroll
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)

(defun +pixel-scroll-interpolate-down (&optional arg)
  (interactive "P")
  (if pixel-scroll-precision-interpolate-page
      (pixel-scroll-precision-interpolate
       (if arg
           (* -1 (prefix-numeric-value arg) (pixel-line-height))
         (- (* 0.75 (window-text-height nil t)))) nil 1)
    (condition-case nil
	(scroll-up arg)
      (end-of-buffer (goto-char (point-max))))))

(defun +pixel-scroll-interpolate-up (&optional arg)
  (interactive "P")
  (if pixel-scroll-precision-interpolate-page
      (pixel-scroll-precision-interpolate
       (if arg
           (* (prefix-numeric-value arg) (pixel-line-height))
         (* 0.75 (window-text-height nil t))) nil 1)
    (condition-case nil
	(scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

;; Help
(use-package which-key
  :ensure nil
  :init
  (which-key-mode))

;; Mode Remap
(setq major-mode-remap-alist
      '((c-mode        . c-ts-mode)
        (c++-mode      . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (python-mode   . python-ts-mode)))

;; Completion
(electric-pair-mode 1)

(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-auto-delay 0.1)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  :init
  (global-corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;; Eglot
(use-package eglot
  :ensure nil
  :hook
  ((c-ts-mode
    c++-ts-mode
    ;; cmake-ts-mode
    python-ts-mode) . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c r" . eglot-rename)
        ("C-c a" . eglot-code-actions)
        ("C-c f" . eglot-format)
        ("C-c o" . eglot-alternatives))
  :config
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=never"
                    "--pch-storage=memory")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode
                 . ("pyright-langserver"
                    "--stdio")))
  (setq eglot-autoshutdown t))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil))

;; Dired
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'global-auto-revert-mode)

;; Project
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

;; Git
(use-package magit
  :bind ("C-x g" . magit-status))

;; TRAMP
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

;; C/C++
(use-package c-ts-mode
  :ensure nil
  :defer t
  :if (treesit-available-p)
  :custom
  (c-ts-mode-indent-style 'bsd)
  (c-ts-mode-indent-offset 4))

(use-package cmake-ts-mode
  :ensure nil
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Common Lisp
(use-package sly
  :defer t
  :commands (sly)
  :custom
  (inferior-lisp-program "ros -Q run"))

;; Python
(use-package python
  :ensure nil
  :defer t)

(use-package flymake-ruff
  :ensure t
  :defer t
  :hook (eglot-managed-mode . flymake-ruff-load))

;; TeX
(use-package tex
  :ensure auctex
  :defer t
  :hook
  ((LaTeX-mode . turn-on-font-lock)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . turn-on-reftex))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil)
  (add-hook 'TeX-mode-hook #'(lambda () (setq fill-column 78))))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command
        '("pandoc" "--from=markdown" "--to=html5"
          "--mathjax" "--standalone"))
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do))
  :custom
  (markdown-open-command "firefox")
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t))

(use-package markdown-toc
  :ensure t
  :defer t
  :custom
  (markdown-toc-header-toc-start "<!-- toc-begin -->")
  (markdown-toc-header-toc-title "")
  (markdown-toc-header-toc-end "<!-- toc-end -->")
  (markdown-toc-preset 'pandoc))

;; Dashboard
(use-package recentf
  :ensure nil
  :init (recentf-mode 1))

(defvar-local sztk-dashboard--was-visible nil)

(use-package dashboard
  :init
  (setq dashboard-projects-backend 'project-el)
  (add-hook 'dashboard-before-initialize-hook #'recentf-mode)
  :config
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-items '((projects . 6)
                          (recents  . 6))
        dashboard-footer-messages '("Esc - Meta - Alt - Ctrl - Shift"))
  (dashboard-setup-startup-hook)

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
               (lambda (b)
                 (when (buffer-live-p b) (kill-buffer b)))
               buf))))
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

;; Terminal
(defun sztk-terminal--run (dir)
  (let* ((dir (expand-file-name dir))
         (default-directory dir)
         (terminals
          `(("konsole" "--new-tab" "--workdir" ,dir)
            ("wt.exe" "-w" "0" "nt"
             "-d" ,(if is-windows (sztk-path-msys2-to-windows dir) dir)
             "-p" "MSYS2-UCRT64"))))
    (unless (and (display-graphic-p)
                 (not (file-remote-p dir))
                 (cl-loop for (exe . args) in terminals
                          when (executable-find exe)
                          return (make-process
                                  :name (concat exe "-from-emacs")
                                  :buffer nil
                                  :command (cons exe args)
                                  :connection-type 'pipe
                                  :noquery t)))
      (shell))))

(defun sztk-terminal-open-here ()
  (interactive)
  (sztk-terminal--run default-directory))

(defun sztk-terminal-open-project ()
  (interactive)
  (if-let ((project (project-current t)))
      (sztk-terminal--run (project-root project))
    (sztk-terminal--run default-directory)))

(keymap-global-set "C-c t" #'sztk-terminal-open-here)

(with-eval-after-load 'project
  (keymap-set project-prefix-map "t" #'sztk-terminal-open-project))

;;; init.el ends here
