(use-package evil
 :ensure t
 :init
   (setq evil-want-C-u-scroll t)
   (setq evil-want-keybinding nil)
   (setq evil-want-minibuffer t)
   (setq evil-search-module 'evil-search)
   (setq evil-vsplit-window-right t)
   (setq evil-split-window-below t)
   (setq evil-move-cursor-back nil)
 :config
   (evil-mode 1)
   (evil-set-initial-state 'Custom-mode 'motion)
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
  (evil-collection-init '(org magit forge mu4e ediff vterm))
  )

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode 1)
 )
(evil-set-undo-system 'undo-tree)

(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  ;; Enable keybinding themes for org-mode
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))

(winner-mode 1)

;; Add winner-mode commands to Evil window map
(define-key evil-window-map "u" 'winner-undo)    ; C-w u
(define-key evil-window-map "U" 'winner-redo)    ; C-w U (shift-u)
