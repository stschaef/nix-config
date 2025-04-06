(use-package flycheck)

(use-package magit
  :ensure t
  :config
  (define-key magit-status-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
  (define-key magit-revision-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
)

(use-package nix-mode
  :mode "\\.nix\\'")
