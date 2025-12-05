(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(defun my-universal-argument-with-leader ()
  "Universal argument that temporarily allows leader key chaining."
  (interactive)
  (let ((general-override-mode nil))  ; Temporarily disable general override
    (universal-argument)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "SPC u") 'universal-argument-more)
       map)
     t)))

(use-package general
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)
  (general-override-mode) ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335
  ;; doomesque hotkeys using spacebar as prefix
  (my-leader-def
    :states '(motion normal visual)
    :keymaps 'override ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335

    ;; map universal argument to SPC-u
    "u" '(my-universal-argument-with-leader :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    "C-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
    "S-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
    "." '(find-file :which-key "Find file")
    ">" '(find-file-other-frame :which-key "Find file (new frame)")
    "," '(consult-buffer :which-key "consult-buffer")
    ":" '(execute-extended-command :which-key "M-x")
    "x" '(scratch-buffer :which-key "Open scratch buffer")
    "d" '(dired-jump :which-key "dired-jump")
    "?" '(counsel-rg :which-key "counsel-rg")
    "v" '(vterm-toggle :which-key "vterm-toggle")
    "m" '(mu4e :which-key "mu4e")

    ;; Config
    "c" '(:ignore t :which-key "Configuration")
    "c k" '((lambda () (interactive) (find-file nix-config-emacs-keybinds))
	      :which-key "Edit keybinds")
    "c r" '(nix-rebuild :which-key "Nix rebuild")

    ;; Search bindings
    "s" '(:ignore t :which-key "search")
    "s p" '(counsel-projectile-rg :which-key "search project")
    "s d" '(counsel-rg :which-key "search directory")
    "s s" '(swiper :which-key "swiper")

    ;; Project bindings
    "p" '(:ignore t :which-key "project")
    "p f" '(counsel-projectile-find-file :which-key "find file in project")
    "p p" '(counsel-projectile-switch-project :which-key "switch project")

    ;; Evil window management
    "w" '(evil-window-map :which-key "Evil window")

    ;; file
    "f" '(:ignore t :which-key "File Management")
    "fr"   '(counsel-recentf :which-key "Recent files")
    "fR"   '(move-this-file :which-key "Move current file")
    "fD"   '(delete-this-file :which-key "Delete current file")
    "fC"   '(copy-this-file :which-key "Copy current file")

    ;; buffer
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "consult-buffer")
    "b[" '(previous-buffer :which-key "Previous buffer")
    "b]" '(next-buffer :which-key "Next buffer")
    "bd" '(kill-current-buffer :which-key "Kill buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
    "br" '(revert-buffer-no-confirm :which-key "Revert buffer")
    "bK" '(kill-other-buffers :which-key "Kill other buffers")
    "by" '(copy-whole-buffer-to-clipboard :which-key "Copy buffer")

    ;; project
    "p" '(:ignore t :which-key "Project")
    "pp" '(projectile-switch-project :which-key "Switch Project")
    "po" '(projectile-find-other-file :which-key "projectile-find-other-file")

    ;; help
    "h" '(:ignore t :which-key "Help")
    "hf" '(describe-function :which-key "describe-function")
    "hk" '(describe-key :which-key "describe-key")
    "hv" '(describe-variable :which-key "describe-variable")
    "ho" '(describe-symbol :which-key "describe-symbol")
    "hm" '(describe-mode :which-key "describe-mode")
    "hF" '(describe-face :which-key "describe-face")
    "hw" '(where-is :which-key "where-is")
    "h." '(display-local-help :which-key "display-local-help")

    ;; toggles
    "t" '(:ignore t :which-key "Toggles")
    "ts" '(flyspell-mode :which-key "flyspell-mode")
    "tv" '(visual-line-mode :which-key "visual-line-mode")

    ;; git
    "g" '(:ignore t :which-key "Magit") ; prefix
    "gg" '(magit-status :which-key "Git status")

    "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
    "qr" '(restart-emacs :which-key "Restart Emacs")

    ;; forester
    "n" '(:ignore t :which-key "Forester")
    "nt" '(forester-new-and-goto :which-key "new tree")
    "nd" '(forester-goto :which-key "goto tree")
    "nc" '(forester-new-and-transclude-and-goto :which-key "transclude new tree")
    )

  ;; magit
  (general-define-key
    ;; https://github.com/emacs-evil/evil-magit/issues/14#issuecomment-626583736
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one)
)
