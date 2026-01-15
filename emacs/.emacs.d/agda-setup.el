(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda --emacs-mode locate")))

(defun setup-agda2-evil-keybindings ()
  "Configure keybindings for working with Agda in Evil mode."
  (evil-define-key 'normal agda2-mode-map
    "gd" 'agda2-goto-definition-keyboard))

(add-hook 'agda2-mode-hook #'setup-agda2-evil-keybindings)

;; Alternative fix for the busy buffer issue
(defun my/agda-fix-process ()
  "Fix Agda process issues."
  (when (and (boundp 'agda2-process-buffer)
             agda2-process-buffer
             (not (buffer-live-p agda2-process-buffer)))
    (setq agda2-process-buffer nil)))

(with-eval-after-load 'agda2-mode
  (add-hook 'agda2-mode-hook 'my/agda-fix-process))
