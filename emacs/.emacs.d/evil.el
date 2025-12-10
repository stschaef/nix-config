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
  :ensure
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
  :ensure t
  :init (evil-visual-mark-mode 1))
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))
;; TODO look into evil embrace

(use-package evil-collection
  :ensure t
  :after (evil magit)
  :config
  (evil-collection-init '(magit forge mu4e))
  )

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode 1)
 )
(evil-set-undo-system 'undo-redo)

(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
