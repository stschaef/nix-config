(use-package consult)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))) ; Enable fuzzy finding
  (setq ivy-initial-inputs-alist nil) ; Remove initial ^ input
  (setq ivy-height 30)
  (setq ivy-extra-directories ())
  (setq ivy-use-selectable-prompt 1)
)

;; Install required packages if not already installed
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :ensure t
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1)

  ;; Custom sorting function that puts dot files at the bottom
  (defun my-prescient-sort-with-dot-files-last (candidates)
    "Sort candidates using prescient, but put dot files at the bottom."
    (let* ((sorted (prescient-sort candidates))
           (dot-files (seq-filter (lambda (c)
                                   (string-match-p "^\\." (file-name-nondirectory c)))
                                 sorted))
           (regular-files (seq-remove (lambda (c)
                                      (string-match-p "^\\." (file-name-nondirectory c)))
                                    sorted)))
      (append regular-files dot-files)))

  ;; Override the default sorting function for buffer switching
  (setf (alist-get 'ivy-switch-buffer ivy-prescient-sort-commands)
        #'my-prescient-sort-with-dot-files-last))

;; Install wgrep package for editable grep buffers
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer nil)
  (setq wgrep-change-readonly-file t))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1)
  )

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package swiper :after ivy)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
)
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(setq counsel-rg-base-command "rg --hidden --vimgrep %s")

(use-package company
  :ensure t
  :config (global-company-mode 1))
(use-package company-box
  :hook (company-mode . company-box-mode))
