(use-package flycheck)

(use-package magit
  :config
  (define-key magit-status-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
  (define-key magit-revision-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
)

(require 'auth-source)
(use-package forge
  :after (magit evil-magit))

(setq auth-sources '("~/.authinfo.gpg"))

(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'lsp-mode)
(add-to-list 'package-selected-packages 'magit-section)

(use-package nix-mode
  :mode "\\.nix\\'")

;; TODO make lean4-mode less brittle
(cond
  ((eq system-type 'darwin)
   (setq lean4-mode-path "/Users/stevenschaefer/lean4-mode")
  )
  ((eq system-type 'gnu/linux)
   (setq lean4-mode-path "/home/steven/lean4-mode")
  )
)

;; TODO make forester.el config less brittle
(cond
  ((eq system-type 'darwin)
   (setq forester-el-path "/Users/stevenschaefer/forester.el")
  )
  ((eq system-type 'gnu/linux)
   (setq forester-el-path "/home/steven/forester.el")
  )
)

;; TODO handle error in forester.el where this variable
;; is nil. As a quick fix make it empty
(setq treesit-language-source-alist ())

;; Add the directory containing forester.el to load-path
(add-to-list 'load-path forester-el-path)

(defun setup-forester-evil-keybindings ()
  "Configure keybindings for working with Agda in Evil mode."
  (evil-define-key 'normal forester-mode-map
    "gd" 'forester-goto))

(add-hook 'forester-mode-hook #'setup-forester-evil-keybindings)

(add-to-list 'load-path lean4-mode-path)

;; Require the package
(require 'forester)

(require 'lean4-mode)

(use-package rust-mode
  :ensure t)

(cond
  ((eq system-type 'darwin)
    (use-package markdown-mode
    :ensure t
    :mode (("README\\.md\\'" . gfm-mode)
	    ("\\.md\\'" . markdown-mode)
	    ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))
    )
)

(use-package tidal
   :ensure t)
(setq tidal-boot-script-path "/nix/store/rdylr02yqhf9530xh7w2cn9wj9a7nm6m-tidal-1.9.5-data/share/ghc-9.8.4/aarch64-osx-ghc-9.8.4/tidal-1.9.5/BootTidal.hs")

(use-package haskell-mode
    :ensure t)

;; Enable undo-tree mode globally
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)

  ;; Enable automatic save/restore of undo history
  (setq undo-tree-auto-save-history t)

  ;; Save undo history to ~/.emacs.d/undo-tree-history/
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))

  ;; Prevent undo-tree files from cluttering your filesystem
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))
