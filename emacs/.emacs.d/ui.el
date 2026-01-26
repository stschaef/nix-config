;;; ui.el --- UI configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; Basic UI Settings (run immediately)
;;; ============================================================

;; These are fast and should run immediately
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)

;; macOS modifier
(setq mac-command-modifier 'meta)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Font configuration (run immediately for proper display)
(set-face-attribute 'default nil
                    :family "JuliaMono"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;; ============================================================
;;; Theme (load immediately for visual feedback)
;;; ============================================================

(use-package ef-themes
  :demand t  ;; Load immediately (override default defer)
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-to-rotate '(ef-cherie ef-summer ef-reverie ef-rosa ef-trio-dark ef-winter ef-day ef-kassio ef-elea-light))
  (modus-themes-load-theme 'ef-rosa))

;;; ============================================================
;;; Modeline (load immediately)
;;; ============================================================

(use-package doom-modeline
  :demand t
  :config
  (doom-modeline-mode 1)
  (doom-modeline-def-modeline 'main
    '(modals matches process buffer-info vcs remote-host buffer-position selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode check))
  (defun my/setup-custom-modeline ()
    (doom-modeline-set-modeline 'main 'default))
  (add-hook 'doom-modeline-mode-hook #'my/setup-custom-modeline))

;;; ============================================================
;;; Dashboard (load on startup)
;;; ============================================================

(use-package dashboard
  :demand t
  :config
  (setq dashboard-projects-switch-function 'projectile-switch-project-by-name)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-sort-function
        (lambda (projects)
          (let ((recent-projects-list (projectile-relevant-known-projects)))
            (if recent-projects-list
                (seq-take recent-projects-list
                          (cdr (assoc 'projects dashboard-items)))
              projects))))
  (setq dashboard-items '((recents . 10)
                          (projects . 10)
                          (bookmarks . 10)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

;;; ============================================================
;;; Deferred UI Packages (load when needed)
;;; ============================================================

;; Ultra-scroll - defer until scrolling
(use-package ultra-scroll
  :commands ultra-scroll-mode
  :init
  (add-hook 'emacs-startup-hook #'ultra-scroll-mode))

;; Rainbow delimiters - only in prog-mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; hl-todo - defer slightly
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Unicode fonts - defer
(use-package unicode-fonts
  :commands unicode-fonts-setup
  :init
  (add-hook 'after-init-hook
            (lambda () (run-with-idle-timer 2 nil #'unicode-fonts-setup))))

;; Org bullets - only in org-mode
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Golden ratio - defer until window switching
(use-package golden-ratio
  :commands golden-ratio-mode
  :init
  (setq golden-ratio-auto-scale t)
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  evil-window-next
                  evil-window-prev
                  evil-window-split
                  evil-window-vsplit
                  evil-window-delete)))
  :hook (emacs-startup . golden-ratio-mode))

;; Anzu - defer until search
(use-package anzu
  :commands (global-anzu-mode anzu-query-replace anzu-query-replace-regexp)
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  :hook (emacs-startup . global-anzu-mode))

(use-package evil-anzu
  :after (evil anzu))

;; Spacious padding - defer slightly
(use-package spacious-padding
  :commands spacious-padding-mode
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 10
          :header-line-width 4
          :mode-line-width 5
          :custom-button-width 3
          :tab-width 4
          :right-divider-width 10
          :scroll-bar-width 8
          :fringe-width 10))
  :hook (emacs-startup . spacious-padding-mode))

;;; ui.el ends here
