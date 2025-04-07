(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

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
    "u" '(universal-argument :which-key "Universal argument")
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

    ;; Search bindings
    "s" '(:ignore t :which-key "search")
    "s p" '(counsel-projectile-rg :which-key "search project")
    "s d" '(counsel-rg :which-key "search directory")
    "s s" '(swiper :which-key "swiper")

    ;; Project bindings
    "p" '(:ignore t :which-key "project")
    "p f" '(counsel-projectile-find-file :which-key "find file in project")
    "p p" '(counsel-projectile-switch-project :which-key "switch project")

    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(vundo :which-key "vundo")
    "ev" '(vundo :which-key "vundo")
    "er" '(query-replace :which-key "query-replace")
    "ep" '(point-to-register :which-key "point-to-register")
    "es" '(consult-register-store :which-key "consult-register-store")
    "ej" '(jump-to-register :which-key "jump-to-register")
    "ef" '(:ignore t :which-key "Fold")
    "efh" '(hs-hide-block :which-key "hs-hide-block")
    "efs" '(hs-show-block :which-key "hs-show-block")
    "efa" '(hs-show-all :which-key "hs-show-all")

    ;; consult
    "c" '(:ignore t :which-key "consult")
    "cf" '(consult-flymake :which-key "consult-flymake")
    "ct" '(consult-theme :which-key "consult-theme")


    "w" evil-window-map
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

    ;; open
    "o" '(:ignore t :which-key "Open")
    "oc" '(open-init-file :which-key "Open init.el")

    ;; project
    "p" '(:ignore t :which-key "Project")
    "pp" '(projectile-switch-project :which-key "Switch Project")
    "po" '(projectile-find-other-file :which-key "projectile-find-other-file")

    ;; help
    "h" '(:ignore t :which-key "Help")
    "hf" '(helpful-callable :which-key "describe-function")
    "hk" '(helpful-key :which-key "describe-key")
    "hv" '(helpful-variable :which-key "describe-variable")
    "ho" '(helpful-symbol :which-key "describe-symbol")
    "hm" '(describe-mode :which-key "describe-mode")
    "hF" '(describe-face :which-key "describe-face")
    "hw" '(where-is :which-key "where-is")
    "h." '(display-local-help :which-key "display-local-help")

    ;; toggles
    "t" '(:ignore t :which-key "Toggles")
    "ts" '(flyspell-mode :which-key "flyspell-mode")
    "tf" '(flyspell-mode :which-key "flyspell-mode")
    "tc" '(flymake-mode :which-key "flymake-mode")
    "tg" '(evil-goggles-mode :which-key "evil-goggles")
    "tI" '(toggle-indent-style :which-key "Indent style")
    "tv" '(visual-line-mode :which-key "visual-line-mode")

    ;; git
    "g" '(:ignore t :which-key "Git") ; prefix
    "gg" '(magit-status :which-key "Git status")

    "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
    "qr" '(restart-emacs :which-key "Restart Emacs")

    ;; forester
    "rn" '(forester-new-and-goto :which-key "Forester new tree")
    "rt" '(forester-new-and-transclude-and-goto :which-key "Forester new tree")
    "rd" '(forester-goto :which-key "Forester go to tree")
    )

  ;; magit
  (general-define-key
    ;; https://github.com/emacs-evil/evil-magit/issues/14#issuecomment-626583736
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one)
)
