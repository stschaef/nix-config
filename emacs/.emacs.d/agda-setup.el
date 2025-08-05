(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda --emacs-mode locate")))

(defun setup-agda2-evil-keybindings ()
  "Configure keybindings for working with Agda in Evil mode."
  (evil-define-key 'normal agda2-mode-map
    "gd" 'agda2-goto-definition-keyboard))

(add-hook 'agda2-mode-hook #'setup-agda2-evil-keybindings)
