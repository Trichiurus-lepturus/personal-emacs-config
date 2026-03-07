;;; lisp/init-ai-assist.el --- AI Assistant -*- lexical-binding: t; -*-

(defvar-local sztk-gptel-key-cache nil)
(use-package gptel
  :defer t
  :bind (:map gptel-mode-map
              ("C-c C-c C-c" . gptel-abort))
  :config
  (defun sztk-gptel-get-key ()
    (or sztk-gptel-key-cache
        (if-let* ((key-file (expand-file-name "ai-assist-api-key.txt"
                                              user-emacs-directory))
                  (exists-p (file-exists-p key-file))
                  (content (with-temp-buffer
                             (insert-file-contents key-file)
                             (string-trim (buffer-string))))
                  (_ (not (string-empty-p content))))
            (setq sztk-gptel-key-cache content)
          (let ((user-input (string-trim
                             (read-string "Enter AI assistant API key: "))))
            (when (and (not (string-empty-p user-input))
                       (yes-or-no-p "Save this API key? "))
              (with-temp-buffer
                (insert user-input)
                (write-region (point-min) (point-max) key-file)
                (set-file-modes key-file #o600))
              (message "API key saved to: %s" key-file))
            (setq sztk-gptel-key-cache user-input)))))
  (setq gptel-model 'deepseek/deepseek-v3.2
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key #'sztk-gptel-get-key
          :models '(deepseek/deepseek-v3.2
                    qwen/qwen3.5-35b-a3b
                    qwen/qwen3.5-397b-a17b
                    z-ai/glm-5
                    google/gemini-3-flash-preview
                    google/gemini-3.1-pro-preview
                    anthropic/claude-sonnet-4.6)))
  (add-hook 'gptel-mode-hook #'gptel-highlight-mode))

(provide 'init-ai-assist)

;;; lisp/init-ai-assist.el ends here
