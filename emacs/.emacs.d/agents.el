;;; agents.el --- AI agent configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; Agent Shell (defer until called)
;;; ============================================================

(use-package agent-shell
  :commands (agent-shell agent-shell-new)
  :config
  ;; Evil state-specific RET behavior
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

  ;; Start diff buffers in Emacs state
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state))))

  ;; OpenCode configuration
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))

  ;; Set OpenCode as the default agent
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config)))

;;; agents.el ends here
