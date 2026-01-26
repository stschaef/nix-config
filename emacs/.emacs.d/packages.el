;;; packages.el --- Package configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; Development Tools
;;; ============================================================

;; Flycheck - defer until prog-mode
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Magit - defer until called
(use-package magit
  :commands (magit-status magit-blame magit-log magit-diff)
  :config
  (define-key magit-status-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
  (define-key magit-revision-mode-map (kbd "<escape>") 'magit-mode-bury-buffer))

;; Auth source
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg"))

;; Forge - defer until magit is loaded
(use-package forge
  :after (magit evil-collection)
  :commands (forge-pull forge-create-pullreq))

;; Register extra packages for installation (they're pulled in by dependencies)
(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'lsp-mode)
(add-to-list 'package-selected-packages 'magit-section)

;;; ============================================================
;;; Language Modes
;;; ============================================================

;; Nix mode - only for .nix files
(use-package nix-mode
  :mode "\\.nix\\'")

;; Rust mode - only for .rs files
(use-package rust-mode
  :mode "\\.rs\\'")

;; Markdown mode - only for .md files (macOS only per original config)
(when (eq system-type 'darwin)
  (use-package markdown-mode
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown")))

;; Haskell mode - only for .hs files
(use-package haskell-mode
  :mode "\\.hs\\'")

;; Tidal - defer until called
(use-package tidal
  :commands (tidal-start-haskell tidal-run-line)
  :config
  (setq tidal-boot-script-path "/nix/store/rdylr02yqhf9530xh7w2cn9wj9a7nm6m-tidal-1.9.5-data/share/ghc-9.8.4/aarch64-osx-ghc-9.8.4/tidal-1.9.5/BootTidal.hs"))

;;; ============================================================
;;; Local Packages (lean4-mode, forester.el)
;;; ============================================================

;; Set paths based on OS
(defvar lean4-mode-path
  (if (eq system-type 'darwin)
      "/Users/stevenschaefer/lean4-mode"
    "/home/steven/lean4-mode"))

(defvar forester-el-path
  (if (eq system-type 'darwin)
      "/Users/stevenschaefer/forester.el"
    "/home/steven/forester.el"))

;; Initialize treesit-language-source-alist to prevent errors
(setq treesit-language-source-alist ())

;; Add to load-path (but don't load yet)
(add-to-list 'load-path forester-el-path)
(add-to-list 'load-path lean4-mode-path)

;; Forester - autoload when .tree files are opened
(autoload 'forester-mode "forester" "Major mode for Forester" t)
(add-to-list 'auto-mode-alist '("\\.tree\\'" . forester-mode))

(with-eval-after-load 'forester
  (defun setup-forester-evil-keybindings ()
    "Configure keybindings for working with Agda in Evil mode."
    (evil-define-key 'normal forester-mode-map
      "gd" 'forester-goto))
  (add-hook 'forester-mode-hook #'setup-forester-evil-keybindings))

;; Lean4 - autoload when .lean files are opened
(autoload 'lean4-mode "lean4-mode" "Major mode for Lean 4" t)
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))

;;; ============================================================
;;; Undo Tree
;;; ============================================================

(use-package undo-tree
  :demand t  ;; Need this early for evil integration
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;;; ============================================================
;;; Terminal - vterm
;;; ============================================================

(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  ;; Start vterm in insert state
  (evil-set-initial-state 'vterm-mode 'insert)

  ;; Disable line numbers in vterm
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))

  ;; Shift+Enter for literal newline (for OpenCode multi-line prompts)
  (define-key vterm-mode-map (kbd "<S-return>")
    (lambda ()
      (interactive)
      (vterm-send-key (kbd "C-j"))))

  ;; M-RET as fallback
  (define-key vterm-mode-map (kbd "M-<return>")
    (lambda ()
      (interactive)
      (vterm-send-key (kbd "C-j"))))

  ;; Use C-c SPC as prefix to access leader key from within vterm
  (with-eval-after-load 'general
    (general-define-key
     :keymaps 'vterm-mode-map
     "C-c SPC" (general-simulate-key "SPC" :state 'normal))))

;;; packages.el ends here
