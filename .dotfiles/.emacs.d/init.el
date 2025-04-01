;;; Initialize package system
(require 'package)

;; Add package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize
(package-initialize)

;;; Initialize package system
(require 'package)

;; Add package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Refresh package list
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Always ensure packages are installed
(setq use-package-always-ensure t)

(use-package org-agenda
 :ensure org)
(use-package evil
 :ensure t
 :init
   (setq evil-want-keybinding nil)
   (setq evil-search-module 'evil-search)
   (setq evil-vsplit-window-right t)
   (setq evil-split-window-below t)
   (setq evil-move-cursor-back nil)
 :config
   (evil-mode 1)
)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package vimish-fold
  :ensure
  :after evil
)
(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))
(use-package evil-visual-mark-mode
  :ensure t)

(use-package auctex)

(use-package flycheck)

(use-package magit
  :ensure t
  :config
  (define-key magit-status-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
  (define-key magit-revision-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
)

(use-package evil-collection
  :ensure t
  :after (evil magit)
  :config
 (evil-collection-init 'magit)) ; This initializes the evil bindings for magit


(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
)

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1))

;; Configure the content of the dashboard
(setq dashboard-items '((recents . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
      ))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package company)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-challenger-deep t))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "JetBrains Mono-16:weight=medium" nil t)
(line-number-mode 1)
(column-number-mode 1)

(use-package general)

(require 'general)
(general-define-key
 :states '(normal visual)
 :keymaps 'override 
 :prefix "SPC"
  "SPC"   '(counsel-M-x :which-key "M-x")
  "."     '(counsel-find-file :which-key "Find file")
  ","     '(ivy-switch-buffer :which-key "Switch Buffer")
  "f r"   '(counsel-recentf :which-key "Recent files")
  "w" evil-window-map
  "s p" `my/projectile-ripgrep
  "p" projectile-mode-map
  "g" magit-mode-map
  "t t"   '(toggle-truncate-lines :which-key "Toggle truncate lines"))
