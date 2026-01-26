;;; evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; Core Evil Setup (must load immediately for keybindings)
;;; ============================================================

(use-package evil
  :demand t  ;; Load immediately - core editing paradigm
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)  ;; Required for evil-collection
  (setq evil-want-minibuffer t)
  (setq evil-search-module 'evil-search)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-move-cursor-back nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'Custom-mode 'motion)
  (evil-set-undo-system 'undo-tree)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)))

;;; ============================================================
;;; Evil Extensions (defer where possible)
;;; ============================================================

;; Evil surround - load after evil
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Vimish fold - defer until needed
(use-package vimish-fold
  :after evil
  :commands (vimish-fold vimish-fold-toggle))

;; Evil vimish fold - hook to modes
(use-package evil-vimish-fold
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;; Evil visual mark mode
(use-package evil-visual-mark-mode
  :after evil
  :config
  (evil-visual-mark-mode 1))

;; Evil lion - for alignment
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

;; Evil collection - defer until magit/other modes load
(use-package evil-collection
  :after evil
  :config
  ;; Only initialize for modes we actually use
  (evil-collection-init '(magit forge vterm ediff dired)))

;; Evil commentary
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

;; Evil org - defer until org-mode
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))

;;; ============================================================
;;; Winner Mode (built-in, fast)
;;; ============================================================

(winner-mode 1)

;; Add winner-mode commands to Evil window map
(with-eval-after-load 'evil
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

;;; evil.el ends here
