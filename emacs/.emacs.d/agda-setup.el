;;; agda-setup.el --- Agda configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; Agda Mode (defer until .agda files)
;;; ============================================================

;; Autoload agda2-mode when opening .agda files
(autoload 'agda2-mode "agda2-mode" "Major mode for Agda" t)
(add-to-list 'auto-mode-alist '("\\.agda\\'" . agda2-mode))
(add-to-list 'auto-mode-alist '("\\.lagda\\.md\\'" . agda2-mode))

;; Load agda-mode location only when needed
(defvar my/agda-mode-loaded nil)

(defun my/ensure-agda-mode ()
  "Load Agda mode if not already loaded."
  (unless my/agda-mode-loaded
    (condition-case err
        (load-file (let ((coding-system-for-read 'utf-8))
                     (shell-command-to-string "agda --emacs-mode locate")))
      (error
       (message "Could not load Agda mode: %s" (error-message-string err))))
    (setq my/agda-mode-loaded t)))

;; Load agda-mode when opening .agda files
(add-hook 'agda2-mode-hook #'my/ensure-agda-mode)

;;; ============================================================
;;; Agda Evil Keybindings
;;; ============================================================

(defun my-agda-goto-definition ()
  "Go to definition and add current position to Evil jump list."
  (interactive)
  (evil-set-jump)
  (agda2-goto-definition-keyboard))

(defun setup-agda2-evil-keybindings ()
  "Configure keybindings for working with Agda in Evil mode."
  (when (featurep 'evil)
    (evil-define-key 'normal agda2-mode-map
      "gd" 'my-agda-goto-definition)))

(with-eval-after-load 'agda2-mode
  (add-hook 'agda2-mode-hook #'setup-agda2-evil-keybindings)

  ;; Fix for busy buffer issue
  (defun my/agda-fix-process ()
    "Fix Agda process issues."
    (when (and (boundp 'agda2-process-buffer)
               agda2-process-buffer
               (not (buffer-live-p agda2-process-buffer)))
      (setq agda2-process-buffer nil)))

  (add-hook 'agda2-mode-hook 'my/agda-fix-process))

;;; agda-setup.el ends here
