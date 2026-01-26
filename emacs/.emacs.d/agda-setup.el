;;; agda-setup.el --- Agda configuration -*- lexical-binding: t; -*-

;; Load agda-mode from the dynamically located path
;; This must run at startup since agda2-mode sets up its own auto-mode-alist
(condition-case err
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda --emacs-mode locate")))
  (error
   (message "Could not load Agda mode: %s" (error-message-string err))))

;; Evil keybindings (deferred until agda2-mode loads)
(defun my-agda-goto-definition ()
  "Go to definition and add current position to Evil jump list."
  (interactive)
  (evil-set-jump)
  (agda2-goto-definition-keyboard))

(defun setup-agda2-evil-keybindings ()
  "Configure keybindings for working with Agda in Evil mode."
  (evil-define-key 'normal agda2-mode-map
    "gd" 'my-agda-goto-definition))

(add-hook 'agda2-mode-hook #'setup-agda2-evil-keybindings)

;; Fix for busy buffer issue
(with-eval-after-load 'agda2-mode
  (defun my/agda-fix-process ()
    "Fix Agda process issues."
    (when (and (boundp 'agda2-process-buffer)
               agda2-process-buffer
               (not (buffer-live-p agda2-process-buffer)))
      (setq agda2-process-buffer nil)))
  (add-hook 'agda2-mode-hook 'my/agda-fix-process))

;; Semantic Agda face mappings to standard font-lock faces
(defun my/agda-map-faces-semantically ()
  "Map Agda faces to semantically equivalent font-lock faces."
  ;; Custom mappings as requested
  (set-face-attribute 'agda2-highlight-field-face nil 
                      :foreground (face-foreground 'font-lock-keyword-face)
                      :weight (face-attribute 'font-lock-keyword-face :weight))
  (set-face-attribute 'agda2-highlight-function-face nil 
                      :foreground (face-foreground 'font-lock-function-name-face)
                      :weight (face-attribute 'font-lock-function-name-face :weight))
  (set-face-attribute 'agda2-highlight-record-face nil 
                      :foreground (face-foreground 'font-lock-builtin-face)
                      :weight (face-attribute 'font-lock-builtin-face :weight))
  (set-face-attribute 'agda2-highlight-bound-variable-face nil 
                      :foreground (face-foreground 'font-lock-string-face)
                      :weight (face-attribute 'font-lock-string-face :weight))
  
  ;; Standard semantic mappings
  (set-face-attribute 'agda2-highlight-keyword-face nil 
                      :foreground (face-foreground 'font-lock-keyword-face)
                      :weight (face-attribute 'font-lock-keyword-face :weight))
  (set-face-attribute 'agda2-highlight-string-face nil 
                      :foreground (face-foreground 'font-lock-string-face)
                      :weight (face-attribute 'font-lock-string-face :weight))
  (set-face-attribute 'agda2-highlight-number-face nil 
                      :foreground (face-foreground 'font-lock-constant-face)
                      :weight (face-attribute 'font-lock-constant-face :weight))
  (set-face-attribute 'agda2-highlight-datatype-face nil 
                      :foreground (face-foreground 'font-lock-type-face)
                      :weight (face-attribute 'font-lock-type-face :weight))
  (set-face-attribute 'agda2-highlight-inductive-constructor-face nil 
                      :foreground (face-foreground 'font-lock-builtin-face)
                      :weight (face-attribute 'font-lock-builtin-face :weight))
  (set-face-attribute 'agda2-highlight-operator-face nil 
                      :foreground (face-foreground 'font-lock-builtin-face)
                      :weight (face-attribute 'font-lock-builtin-face :weight))
  (set-face-attribute 'agda2-highlight-module-face nil 
                      :foreground (face-foreground 'font-lock-constant-face)
                      :weight 'bold)
  (set-face-attribute 'agda2-highlight-symbol-face nil 
                      :foreground (face-foreground 'font-lock-variable-name-face)
                      :weight (face-attribute 'font-lock-variable-name-face :weight))
  (set-face-attribute 'agda2-highlight-primitive-face nil 
                      :foreground (face-foreground 'font-lock-builtin-face)
                      :weight 'bold)
  (set-face-attribute 'agda2-highlight-postulate-face nil 
                      :foreground (face-foreground 'font-lock-warning-face)
                      :weight 'bold)
  (set-face-attribute 'agda2-highlight-generalizable-variable-face nil 
                      :foreground (face-foreground 'font-lock-variable-name-face)
                      :weight (face-attribute 'font-lock-variable-name-face :weight))
  (set-face-attribute 'agda2-highlight-error-face nil 
                      :background (face-background 'error nil t)
                      :foreground (face-foreground 'error))
  (set-face-attribute 'agda2-highlight-typechecks-face nil 
                      :background (face-background 'success nil t)
                      :foreground (face-foreground 'success)))

;; Apply face mappings when theme changes or Agda mode loads
(add-hook 'agda2-mode-hook #'my/agda-map-faces-semantically)
(add-hook 'ef-themes-post-load-hook #'my/agda-map-faces-semantically)

;;; agda-setup.el ends here
