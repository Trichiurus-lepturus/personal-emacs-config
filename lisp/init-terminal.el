;;; lisp/init-terminal.el --- Terminal -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'init-const)

(defun sztk-path-msys2-to-windows (path)
  (let ((p (expand-file-name path)))
    (if (executable-find "cygpath")
        (string-trim (shell-command-to-string
                      (format "cygpath -w \"%s\"" p)))
      (convert-standard-filename p))))

(defun sztk-terminal--run (dir)
  (let* ((dir (expand-file-name dir))
         (default-directory dir)
         (terminals
          `(("konsole" "--new-tab" "--workdir" ,dir)
            ("wt.exe" "-w" "0" "nt"
             "-d" ,(if is-windows (sztk-path-msys2-to-windows dir) dir)
             "-p" "MSYS2-UCRT64"))))
    (unless (and (display-graphic-p)
                 (not (file-remote-p dir))
                 (cl-loop for (exe . args) in terminals
                          when (executable-find exe)
                          return (make-process
                                  :name (concat exe "-from-emacs")
                                  :buffer nil
                                  :command (cons exe args)
                                  :connection-type 'pipe
                                  :noquery t)))
      (shell))))

(defun sztk-terminal-open-here ()
  (interactive)
  (sztk-terminal--run default-directory))

(defun sztk-terminal-open-project ()
  (interactive)
  (if-let ((project (project-current t)))
      (sztk-terminal--run (project-root project))
    (sztk-terminal--run default-directory)))

(keymap-global-set "C-c t" #'sztk-terminal-open-here)

(with-eval-after-load 'project
  (keymap-set project-prefix-map "t" #'sztk-terminal-open-project))

(provide 'init-terminal)

;;; lisp/init-terminal.el ends here
