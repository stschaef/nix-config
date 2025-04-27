(use-package flycheck)

(use-package magit
  :ensure t
  :config
  (define-key magit-status-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
  (define-key magit-revision-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
)

(use-package nix-mode
  :mode "\\.nix\\'")

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

;; Require the package
(require 'forester)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
