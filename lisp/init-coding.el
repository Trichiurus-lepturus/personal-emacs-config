;;; lisp/init-coding.el --- Coding -*- lexical-binding: t; -*-

(setq major-mode-remap-alist
      '((c-mode        . c-ts-mode)
        (c++-mode      . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (python-mode   . python-ts-mode)))

(use-package eglot
  :ensure nil
  :hook
  ((c-ts-mode
    c++-ts-mode
    cmake-ts-mode
    python-ts-mode) . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c r" . eglot-rename)
        ("C-c a" . eglot-code-actions)
        ("C-c f" . eglot-format)
        ("C-c C-d" . eglot-find-declaration)
        ("C-c C-i" . eglot-find-implementation)
        ("C-c C-t" . eglot-find-typeDefinition))
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
               '(cmake-ts-mode
                 . ("neocmakelsp" "stdio")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode
                 . ("pyright-langserver"
                    "--stdio")))
  (setq eglot-autoshutdown t))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package flymake
  :ensure nil
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)
        ("C-c !" . flymake-show-buffer-diagnostics)))

(use-package c-ts-mode
  :ensure nil
  :defer t
  :custom
  (c-ts-mode-indent-style 'bsd)
  (c-ts-mode-indent-offset 4))

(use-package cmake-ts-mode
  :ensure nil
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package sly
  :defer t
  :commands (sly)
  :custom
  (inferior-lisp-program
   (if (executable-find "ros")
       "ros -Q run"
     "sbcl")))

(use-package elisp
  :ensure nil
  :hook (emacs-lisp-mode . flymake-mode))

(use-package python
  :ensure nil
  :defer t)

(use-package flymake-ruff
  :defer t
  :hook (eglot-managed-mode . flymake-ruff-load))

(provide 'init-coding)

;;; lisp/init-coding.el ends here
