;; Vertico: Fast and minimal vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)   ; Cycle from bottom to top
  (setq vertico-count 20)) ; Show more candidates

;; Orderless: Powerful completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion))
     (eglot (styles orderless))
     (eglot-capf (styles orderless))))
  :config
  ;; Make shorter matches rank higher
  (setq orderless-matching-styles '(orderless-literal
                                     orderless-prefixes
                                     orderless-initialism
                                     orderless-regexp)))

;; Marginalia: Rich annotations in the minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))) ; Cycle annotation styles

;; Consult: Enhanced commands with previews
(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)

         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)

         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)

         ;; Other custom bindings
         ("M-y" . consult-yank-pop)

         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-r" . consult-history))

  :config
  ;; Preview settings
  (setq consult-preview-key 'any)  ; Preview immediately
  (setq consult-narrow-key "<")    ; Use < to narrow

  ;; Use ripgrep with hidden files
  (setq consult-ripgrep-args
        "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

;; Embark: Contextual actions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ; Pick an action
   ("C-;" . embark-dwim)        ; Do what I mean
   ("C-h B" . embark-bindings)) ; Alternative for describe-bindings
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark integration with Consult
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; wgrep: Edit grep results directly
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer nil)
  (setq wgrep-change-readonly-file t))

;; Recent files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" my/data-directory))
  (setq recentf-max-saved-items 200)
  (recentf-mode 1))

;; Projectile with Vertico
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default) ; Use Vertico
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" my/data-directory))
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" my/data-directory)))

;; Company
(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; ===== SAVEHIST WITH PERSISTENT DIRECTORY =====
(use-package savehist
  :init
  (setq savehist-file (expand-file-name "savehist" my/data-directory))
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        '(command-history
          extended-command-history
          compile-command
          kill-ring
          regexp-search-ring
          search-ring)))

;; ===== CUSTOM MINIBUFFER BEHAVIORS =====

;; 1. Backspace deletes up to "/" in file paths
(defun my/minibuffer-backward-delete-to-slash ()
  "Delete backward in minibuffer up to and including the last slash."
  (interactive)
  (if minibuffer-completing-file-name
      (if (looking-back "/" (minibuffer-prompt-end))
          (delete-char -1)
        (delete-region (point)
                       (save-excursion
                         (skip-chars-backward "^/")
                         (point))))
    (delete-backward-char 1)))

(define-key minibuffer-local-filename-completion-map
            (kbd "DEL") 'my/minibuffer-backward-delete-to-slash)


;; Add a custom binding for quick export
(with-eval-after-load 'embark-consult
  (define-key consult-mode-map (kbd "C-c C-e") 'embark-export))
