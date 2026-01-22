(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(ultra-scroll-mode 1)

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
		    :family "JuliaMono"
		    :height 140
		    :weight 'normal
		    :width 'normal)

(use-package hl-todo :init (global-hl-todo-mode 1))
(use-package unicode-fonts :ensure t)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config

  ;; Define your custom modeline layout
  (doom-modeline-def-modeline 'main
    '(modals matches process buffer-info vcs remote-host buffer-position selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode check))

  ;; Set it as the default
  (defun my/setup-custom-modeline ()
    (doom-modeline-set-modeline 'main 'default))

  (add-hook 'doom-modeline-mode-hook #'my/setup-custom-modeline))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  (setq modus-themes-to-rotate '(ef-cherie ef-summer ef-reverie ef-rosa ef-trio-dark ef-winter ef-day ef-kassio ef-elea-light))

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-rosa))

;;light
;; ef-summer, ef-reverie
;; dark
;; ef-rosa, ef-trio-dark, ef-cherie, ef-winter

;; (use-package doom-themes
;;   :ensure t;;   :config
;;   (load-theme 'modus-vivendi t))
;; challenger deep
;; Isovkem
;; feather dark
;; tokyo night

(use-package org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-display-line-numbers-mode)

;; Try to avoid thrashing
(setq gc-cons-threshold (* 16 1024 1024))

(use-package golden-ratio
  :ensure t
  :init
  (setq golden-ratio-auto-scale t)     ;; optional: scales to frame size
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  evil-window-next       ;; C-w w
                  evil-window-prev       ;; C-w W
                  evil-window-split
                  evil-window-vsplit
                  evil-window-delete)))
  (golden-ratio-mode 1))

(use-package anzu :ensure t
  :init
  (global-anzu-mode +1)
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000))

(use-package evil-anzu :ensure t :after (evil anzu))

;; (custom-set-faces
;;  `(anzu-mode-line ((t (:foreground ,(my/catppuccin-get-color 'sapphire) :weight bold)))) `(anzu-mode-line-no-match ((t (:foreground ,(my/catppuccin-get-color 'red) :weight bold)))))

(use-package spacious-padding
  :ensure t
  :config
  ;; These are the default values, but I keep them here for visibility.
  ;; Also check `spacious-padding-subtle-frame-lines'.
  (setq spacious-padding-widths
        '( :internal-border-width 10
           :header-line-width 4
           :mode-line-width 5
           :custom-button-width 3
           :tab-width 4
           :right-divider-width 10
           :scroll-bar-width 8
           :fringe-width 10))

  (spacious-padding-mode 1)
  )
