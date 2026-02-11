;;; lisp/init-package.el --- Package -*- lexical-binding: t; -*-

(require 'package)
(require 'init-const)

(when is-windows
  (setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(provide 'init-package)

;;; lisp/init-package.el ends here
