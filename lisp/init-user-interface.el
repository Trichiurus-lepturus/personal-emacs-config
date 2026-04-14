;;; lisp/init-user-interface.el --- UI -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq-default indent-tabs-mode nil
              fill-column 76
              require-final-newline t)

(setq default-input-method nil)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      use-dialog-box nil)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(global-font-lock-mode 1)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom (repeat-exit-timeout 3.0))

(use-package winner
  :ensure nil
  :init (winner-mode 1))

(use-package diminish)

(use-package faces
  :ensure nil
  :preface
  (defun sztk-setup-fonts (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (when (display-graphic-p)
        (set-face-attribute 'default nil
                            :family "Fira Mono" :height 120)
        (set-face-attribute 'fixed-pitch nil
                            :family "Fira Mono" :inherit 'default)
        (dolist (pair '((han . "Sarasa Mono SC")
                        (kana . "Sarasa Mono J")
                        (hangul . "Sarasa Mono K")
                        (bopomofo . "Sarasa Mono TC")
                        (cjk-misc . "Sarasa Mono SC")))
          (set-fontset-font t (car pair) (font-spec :family (cdr pair)))
          (add-to-list 'face-font-rescale-alist (cons (cdr pair) 1.0)))
        (let ((emoji-font "Noto Color Emoji"))
          (set-fontset-font t 'emoji (font-spec :family emoji-font))
          (add-to-list 'face-font-rescale-alist (cons emoji-font 1.21))))))
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'sztk-setup-fonts)
    (sztk-setup-fonts)))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(defun sztk-adv-preserve-background (orig-fun theme &rest args)
  (cl-flet ((anchor-background ()
              (when-let* ((_ (and (display-graphic-p)
                                  (frame-live-p (selected-frame))))
                          (bg (face-background 'default)))
                (set-face-background 'default bg))))
    (anchor-background)
    (apply orig-fun theme args)
    (anchor-background)))
(advice-add 'load-theme :around #'sztk-adv-preserve-background)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-nord-aurora t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(defvar sztk-theme-circle '(doom-nord-aurora doom-gruvbox))
(defun sztk-toggle-theme ()
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (next-theme (or (cadr (member current-theme sztk-theme-circle))
                         (car sztk-theme-circle))))
    (load-theme next-theme t)
    (mapc #'disable-theme (cdr custom-enabled-themes))
    (message "Theme: %s" next-theme)))
(keymap-global-set "C-c y" #'sztk-toggle-theme)

(use-package rainbow-delimiters
  :defer t
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
  :diminish
  :init
  (which-key-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(provide 'init-user-interface)

;;; lisp/init-user-interface.el ends here
