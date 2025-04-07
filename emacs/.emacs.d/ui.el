(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)

(global-auto-revert-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq mac-command-modifier 'meta)

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-switch-function 'projectile-switch-project-by-name)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-sort-function
        (lambda (projects)
          (let ((recent-projects-list (projectile-relevant-known-projects)))
            (if recent-projects-list
                ;; Return only the most recent ones (limited by dashboard-items setting)
                (seq-take recent-projects-list
                          (cdr (assoc 'projects dashboard-items)))
              projects))))
  (setq dashboard-items '((recents . 10)
                          (projects . 10)
                          (bookmarks . 10)
			  ))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
)

(set-face-attribute 'default nil
		    :family "JetBrainsMono Nerd Font"
		    :height 120
		    :weight 'normal
		    :width 'normal)

(use-package hl-todo :ensure t :init (global-hl-todo-mode 1))
(use-package unicode-fonts :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload)

;; (Use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-challenger-deep t))
