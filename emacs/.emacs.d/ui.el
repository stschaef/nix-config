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
(setq catppuccin-flavor 'frappe) ;; 'latte, 'frappe, 'macchiato, or 'mocha
(catppuccin-reload)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-challenger-deep t))

(defun my/catppuccin-get-color (color)
  "Get a color from the current Catppuccin theme."
  (let ((colors (pcase catppuccin-flavor
                  ('latte catppuccin-latte-colors)
                  ('frappe catppuccin-frappe-colors)
                  ('macchiato catppuccin-macchiato-colors)
                  ('mocha catppuccin-mocha-colors))))
    (alist-get color colors)))

(custom-set-faces
 `(agda2-highlight-datatype-face ((t (:foreground ,(my/catppuccin-get-color 'sapphire)))))
 `(agda2-highlight-inductive-constructor-face ((t (:foreground ,(my/catppuccin-get-color 'sapphire)))))
 `(agda2-highlight-function-face ((t (:foreground ,(my/catppuccin-get-color 'rosewater)))))
 `(agda2-highlight-keyword-face ((t (:foreground ,(my/catppuccin-get-color 'red)))))
 `(agda2-highlight-module-face ((t (:foreground ,(my/catppuccin-get-color 'blue)))))
 `(agda2-highlight-symbol-face ((t (:foreground ,(my/catppuccin-get-color 'lavender)))))
 `(agda2-highlight-postulate-face ((t (:foreground ,(my/catppuccin-get-color 'blue)))))
 `(agda2-highlight-bound-variable-face ((t (:foreground ,(my/catppuccin-get-color 'pink)))))
 `(agda2-highlight-generalizable-variable-face ((t (:foreground ,(my/catppuccin-get-color 'maroon)))))
 `(agda2-highlight-primitive-face ((t (:foreground ,(my/catppuccin-get-color 'peach)))))
 `(agda2-highlight-number-face ((t (:foreground ,(my/catppuccin-get-color 'peach)))))
 `(agda2-highlight-string-face ((t (:foreground ,(my/catppuccin-get-color 'green)))))
 `(agda2-highlight-operator-face ((t (:foreground ,(my/catppuccin-get-color 'teal)))))
 `(agda2-highlight-error-face ((t (:foreground ,(my/catppuccin-get-color 'green)))))
 `(agda2-highlight-typechecks-face ((t (:foreground ,(my/catppuccin-get-color 'green)))))
)

(use-package org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-display-line-numbers-mode)
