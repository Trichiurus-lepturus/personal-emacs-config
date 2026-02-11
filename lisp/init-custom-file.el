;;; lisp/init-custom-file.el --- Custom -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-custom-file)

;;; lisp/init-custom-file.el ends here
