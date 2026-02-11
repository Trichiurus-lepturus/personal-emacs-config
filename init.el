;;; init.el --- Main Emacs Config -*- lexical-binding: t; -*-

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'init-const)
(require 'init-package)
(require 'init-fundamental)

(require 'init-user-interface)
(require 'init-completion)
(require 'init-project)
(require 'init-coding)
(require 'init-markup)

(require 'init-dashboard)
(require 'init-terminal)
(require 'init-custom-file)

;;; init.el ends here
