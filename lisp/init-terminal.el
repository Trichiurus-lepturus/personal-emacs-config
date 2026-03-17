;;; lisp/init-terminal.el --- Terminal -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'init-const)

(use-package vterm
  :defer t
  :hook
  ((vterm-mode . (lambda ()
                   (setq-local global-hl-line-mode nil)
                   (global-hl-line-unhighlight)))
   (vterm-copy-mode . (lambda ()
                        (if vterm-copy-mode
                            (hl-line-mode 1)
                          (hl-line-mode -1))))))

(defun sztk-path-msys2-to-windows (path)
  (let ((p (expand-file-name path)))
    (if (executable-find "cygpath")
        (string-trim (shell-command-to-string
                      (format "cygpath -w \"%s\"" p)))
      (convert-standard-filename p))))

(defun sztk-terminal--run (directory)
  (cl-flet ((external-p (dir)
              (and (display-graphic-p) (not (file-remote-p dir))))
            (konsole-p () (executable-find "konsole"))
            (wt-p () (executable-find "wt.exe"))
            (wt-nt-p ()
              (let* ((ps-script "Get-Process WindowsTerminal \
-ErrorAction SilentlyContinue | Where-Object {$_.MainWindowHandle -ne 0}")
                     (cmd (concat "powershell.exe -NoProfile \
-NonInteractive -Command " (shell-quote-argument ps-script)))
                     (output (shell-command-to-string cmd)))
                (not (string-empty-p (string-trim output)))))
            (expand (dir)
              (let ((dir (expand-file-name dir)))
                (if windows-p (sztk-path-msys2-to-windows dir) dir))))
    (if-let ((exterminal
              (and (external-p directory)
                   (cond
                    ((konsole-p)
                     `("konsole" "--new-tab" "--workdir" ,(expand directory)))
                    ((wt-p)
                     `("cmd.exe" "/c" "start" "" "wt.exe"
                       ,@(when (wt-nt-p) '("-w" "0" "nt"))
                       "-d" ,(expand directory) "-p" "MSYS2-UCRT64"))
                    (t nil)))))
        (make-process :name (car exterminal)
                      :buffer nil
                      :command exterminal
                      :connection-type 'pipe
                      :noquery t)
      (let* ((dir-name (file-name-nondirectory
                        (directory-file-name directory)))
             (vterm-name (format "*vterm-%s*" dir-name)))
        (vterm (generate-new-buffer-name vterm-name))))))

(defun sztk-terminal-open-here ()
  (interactive)
  (sztk-terminal--run default-directory))

(defun sztk-terminal-open-project ()
  (interactive)
  (if-let ((project (project-current nil)))
      (sztk-terminal--run (project-root project))
    (sztk-terminal--run default-directory)))

(keymap-global-set "C-c t" #'sztk-terminal-open-here)

(with-eval-after-load 'project
  (keymap-set project-prefix-map "t" #'sztk-terminal-open-project))

(provide 'init-terminal)

;;; lisp/init-terminal.el ends here
