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
        TeX-master nil
        TeX-check-TeX nil
        TeX-show-compilation t)
  (setq-default TeX-engine 'xetex)
  (add-hook 'TeX-mode-hook #'(lambda () (setq fill-column 78))))

(use-package markdown-mode
  :defer t
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
  :defer t
  :custom
  (markdown-toc-header-toc-start "<!-- toc-begin -->")
  (markdown-toc-header-toc-title "")
  (markdown-toc-header-toc-end "<!-- toc-end -->")
  (markdown-toc-preset 'pandoc))

(use-package org
  :ensure nil
  :defer t
  :preface
  (defvar sztk-org-table-font "Sarasa Mono SC")
  (defun sztk-org-table-align-fix-advice (orig-fun &rest args)
    (let ((m-buf (get-buffer-create " *Org string width*")))
      (with-current-buffer m-buf
        (setq-local face-remapping-alist
                    `((org-table :family ,sztk-org-table-font)))))
    (apply orig-fun args))
  :init
  (advice-add 'org-string-width :around #'sztk-org-table-align-fix-advice)
  :bind (:map org-mode-map
              ("C-'" . nil))
  :hook
  (org-mode . (lambda () (face-remap-add-relative
                          'org-table :family sztk-org-table-font)))
  :custom
  (org-link-descriptive nil))

(use-package pdf-tools
  :preface
  (defun sztk-pdf-tools-init-once (frame)
    (when (display-graphic-p frame)
      (pdf-loader-install)
      (remove-hook 'after-make-frame-functions #'sztk-pdf-tools-init-once)))
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'sztk-pdf-tools-init-once)
    (when (display-graphic-p)
      (pdf-loader-install))))

(provide 'init-markup)

;;; lisp/init-markup.el ends here
