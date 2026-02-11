;;; lisp/init-completion.el --- Completion -*- lexical-binding: t; -*-

(electric-pair-mode 1)

(use-package vertico
  :init
  (vertico-mode 1))

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
  (global-corfu-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(provide 'init-completion)

;;; lisp/init-completion.el ends here
