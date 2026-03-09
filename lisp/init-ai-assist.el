;;; lisp/init-ai-assist.el --- AI Assistant -*- lexical-binding: t; -*-

(defun sztk-gptel-get-key ()
  (when-let ((match (auth-source-search
                     :host "openrouter.ai"
                     :user "apikey")))
    (let ((secret (plist-get (car match) :secret)))
      (if (functionp secret) (funcall secret) secret))))
(use-package gptel
  :defer t
  :config
  (setf (alist-get 'default gptel-directives)
        "You are a large language model living in Emacs \
and a helpful assistant. Respond concisely using org-mode syntax.")
  (setq gptel-default-mode 'org-mode
        gptel-model 'deepseek/deepseek-v3.2
        gptel-use-tools nil
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
