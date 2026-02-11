;;; lisp/init-markup.el --- Markup Languages -*- lexical-binding: t; -*-

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

(provide 'init-markup)

;;; lisp/init-markup.el ends here
