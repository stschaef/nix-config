;;; search-and-completion.el --- Completion and search configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; Vertico - Fast vertical completion (load immediately)
;;; ============================================================

(use-package vertico
  :demand t
  :init
  ;; Ensure backtrace is loaded before vertico (fixes Emacs 30 compatibility)
  (require 'backtrace nil t)
  :config
  (vertico-mode)
  (setq vertico-cycle t)
  (setq vertico-count 20))

;; Make vertico minibuffer navigation work nicely with Evil's normal state.
;; Allow arrow keys, j/k, and RET to navigate and select completions while
;; in normal mode (and insert mode still works as before).
(with-eval-after-load 'vertico
  ;; j/k should only act as navigation when Evil is in normal state; in
  ;; insert state they must insert the characters so you can type normally.
  (defun my/vertico-next-or-self-insert ()
    (interactive)
    (if (and (boundp 'evil-state) (eq evil-state 'normal))
        (vertico-next)
      (self-insert-command 1)))

  (defun my/vertico-previous-or-self-insert ()
    (interactive)
    (if (and (boundp 'evil-state) (eq evil-state 'normal))
        (vertico-previous)
      (self-insert-command 1)))

  (define-key vertico-map (kbd "<down>") #'vertico-next)
  (define-key vertico-map (kbd "<up>") #'vertico-previous)
  (define-key vertico-map (kbd "j") #'my/vertico-next-or-self-insert)
  (define-key vertico-map (kbd "k") #'my/vertico-previous-or-self-insert)
  (define-key vertico-map (kbd "RET") #'vertico-exit)
  (define-key vertico-map (kbd "<return>") #'vertico-exit))

;; If Evil is loaded, make sure these bindings are active in Evil's normal
;; state inside the minibuffer. Evil can shadow `vertico-map` bindings, so
;; define them specifically for `normal` state.
(with-eval-after-load 'evil
  (when (boundp 'vertico-map)
    ;; Only define j/k in evil normal state; insert state will still
    ;; insert characters because we defined helper commands above that
    ;; call `self-insert-command` when not in normal state.
    (evil-define-key 'normal vertico-map
      (kbd "<down>") #'vertico-next
      (kbd "<up>") #'vertico-previous
      (kbd "j") #'my/vertico-next-or-self-insert
      (kbd "k") #'my/vertico-previous-or-self-insert
      (kbd "RET") #'vertico-exit
      (kbd "<return>") #'vertico-exit)))

;; Vertico Directory extension
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;; ============================================================
;;; Orderless - Completion style (load immediately)
;;; ============================================================

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion))
     (eglot (styles orderless))
     (eglot-capf (styles orderless))))
  :config
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-prefixes
                                    orderless-initialism
                                    orderless-regexp)))

;;; ============================================================
;;; Marginalia - Rich annotations (load immediately)
;;; ============================================================

(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;;; ============================================================
;;; Consult - Enhanced commands (defer until called)
;;; ============================================================

(use-package consult
  :commands (consult-buffer consult-find consult-grep consult-ripgrep
             consult-line consult-imenu consult-bookmark consult-recent-file
             consult-yank-pop consult-goto-line consult-outline consult-mark
             consult-history consult-mode-command consult-kmacro)
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
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<")
  (setq consult-ripgrep-args
        "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

(defun my/buffer-sort-by-mode (buffers)
  "Sort BUFFERS by major-mode name, then alphabetically."
  (sort buffers
        (lambda (a b)
          (let ((mode-a (symbol-name (buffer-local-value 'major-mode (get-buffer a))))
                (mode-b (symbol-name (buffer-local-value 'major-mode (get-buffer b)))))
            (if (string= mode-a mode-b)
                (string< a b)
              (string< mode-a mode-b)))))
  (setq consult--source-buffer
      (plist-put consult--source-buffer :items
                 (lambda ()
                   (my/buffer-sort-by-mode
                    (consult--buffer-query :sort 'alpha
                                           :as #'buffer-name))))

;;; ============================================================
;;; Embark - Contextual actions (defer until called)
;;; ============================================================

(use-package embark
  :commands (embark-act embark-dwim embark-bindings)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ============================================================
;;; Wgrep - Edit grep results (defer until needed)
;;; ============================================================

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer nil)
  (setq wgrep-change-readonly-file t))

;;; ============================================================
;;; Recentf - Recent files (load immediately, lightweight)
;;; ============================================================

(use-package recentf
  :ensure nil  ;; Built-in
  :demand t
  :config
  (setq recentf-max-saved-items 200)
  (recentf-mode 1))

;;; ============================================================
;;; Projectile (defer config, enable mode)
;;; ============================================================

(use-package projectile
  :commands (projectile-find-file projectile-switch-project
             projectile-find-other-file projectile-relevant-known-projects)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'default)
  :init
  (projectile-mode +1))

;;; ============================================================
;;; Company - Completion (defer until typing)
;;; ============================================================

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; ============================================================
;;; Savehist - Persistent history (load immediately)
;;; ============================================================

(use-package savehist
  :ensure nil  ;; Built-in
  :demand t
  :config
  (setq savehist-additional-variables
        '(command-history
          extended-command-history
          compile-command
          kill-ring
          regexp-search-ring
          search-ring))
  (savehist-mode 1))

;;; ============================================================
;;; Custom Commands
;;; ============================================================

(defun my/consult-ripgrep-current-dir ()
  "Run consult-ripgrep starting from the current directory."
  (interactive)
  (consult-ripgrep default-directory))

;;; search-and-completion.el ends here
