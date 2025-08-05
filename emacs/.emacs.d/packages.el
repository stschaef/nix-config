(use-package flycheck)

(use-package magit
  :ensure t
  :config
  (define-key magit-status-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
  (define-key magit-revision-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
)

;; TODO forge for PRs
;; (use-package forge
;;   :after magit)

(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'lsp-mode)
(add-to-list 'package-selected-packages 'magit-section)

(package-refresh-contents)
(package-install-selected-packages 'no-confirm)

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
