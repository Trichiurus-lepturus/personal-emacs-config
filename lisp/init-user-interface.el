;;; init-user-interface.el --- UI -*- lexical-binding: t; -*-

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package which-key
  :ensure nil
  :init
  (which-key-mode))

(use-package avy
  :bind
  (("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.2))

(provide 'init-user-interface)

;;; lisp/init-user-interface.el ends here
