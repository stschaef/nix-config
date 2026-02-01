;;; keybinds.el --- Keybinding configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; Which-key (load immediately for discoverability)
;;; ============================================================

(use-package which-key
  :demand t
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun my-universal-argument-with-leader ()
  "Universal argument that temporarily allows leader key chaining."
  (interactive)
  (let ((general-override-mode nil))
    (universal-argument)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "SPC u") 'universal-argument-more)
       map)
     t)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;;; ============================================================
;;; General.el Keybindings
;;; ============================================================

(use-package general
  :demand t  ;; Need this immediately for leader key
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)
  (general-override-mode)

  ;; Doomesque hotkeys using spacebar as prefix
  (my-leader-def
    :states '(motion normal visual)
    :keymaps 'override

    ;; Universal argument
    "u" '(my-universal-argument-with-leader :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")

    "e" '(:ignore t :which-key "Eval")
    "er" '(eval-region :which-key "eval-region")
    "eb" '(eval-buffer :which-key "eval-buffer")

    ;; Files
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    "S-SPC" '(projectile-find-file-other-window :which-key "Projectile find file (new window)")
    "C-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
    "." '(find-file :which-key "Find file")
    ">" '(find-file-other-window :which-key "Find file (new window)")
    "C-." '(find-file-other-frame :which-key "Find file (new frame)")

    ;; Buffers and commands
    "," '(consult-buffer :which-key "Switch buffer")
    ":" '(execute-extended-command :which-key "M-x")
    "x" '(scratch-buffer :which-key "Open scratch buffer")
    "d" '(dired-jump :which-key "dired-jump")
    "?" '(consult-ripgrep :which-key "Ripgrep search")

    ;; Config
    "c" '(:ignore t :which-key "Configuration")
    "cn" '(:ignore t :which-key "Configure Nix")
    "ce" '(:ignore t :which-key "Configure Emacs")
    "ct" '(:ignore t :which-key "Toggles")
    "cei" '((lambda () (interactive) (find-file nix-config-emacs-init))
            :which-key "Edit init")
    "cek" '((lambda () (interactive) (find-file nix-config-emacs-keybinds))
            :which-key "Edit keybinds")
    "cep" '((lambda () (interactive) (find-file nix-config-emacs-packages))
            :which-key "Edit packages")
    "cnf" '((lambda () (interactive) (find-file nix-config-flake))
            :which-key "Edit Nix flake")
    "cnp" '((lambda () (interactive) (find-file nix-config-packages))
            :which-key "Edit Nix packages")
    "cnr" '(nix-rebuild :which-key "Nix rebuild")
    "cts" '(flyspell-mode :which-key "flyspell-mode")
    "ctv" '(visual-line-mode :which-key "visual-line-mode")
    "ctf" '(auto-fill-mode :which-key "auto-fill-mode")

    ;; Search bindings
    "s" '(:ignore t :which-key "search")
    "s p" '(consult-ripgrep :which-key "search project")
    "s d" '(my/consult-ripgrep-current-dir :which-key "search directory")
    "s s" '(consult-line :which-key "search buffer")
    "s l" '(consult-line :which-key "search lines")
    "s g" '(consult-grep :which-key "grep")
    "s i" '(consult-imenu :which-key "imenu")

    ;; Project bindings
    "p" '(:ignore t :which-key "Project")
    "p f" '(projectile-find-file :which-key "find file in project")
    "p p" '(projectile-switch-project :which-key "switch project")
    "p o" '(projectile-find-other-file :which-key "find other file")

    ;; Evil window management
    "w" '(evil-window-map :which-key "Evil window")

    ;; File management
    "f" '(:ignore t :which-key "File Management")
    "f r" '(consult-recent-file :which-key "Recent files")
    "f R" '(move-this-file :which-key "Move current file")
    "f D" '(delete-this-file :which-key "Delete current file")
    "f C" '(copy-this-file :which-key "Copy current file")

    ;; Buffer management
    "b" '(:ignore t :which-key "Buffer")
    "b e" '(eval-buffer :which-key "Eval buffer")
    "b b" '(consult-buffer :which-key "Switch buffer")
    "b [" '(previous-buffer :which-key "Previous buffer")
    "b ]" '(next-buffer :which-key "Next buffer")
    "b d" '(kill-current-buffer :which-key "Kill buffer")
    "b k" '(kill-current-buffer :which-key "Kill buffer")
    "b l" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
    "b r" '(revert-buffer-no-confirm :which-key "Revert buffer")
    "b K" '(kill-other-buffers :which-key "Kill other buffers")
    "b y" '(copy-whole-buffer-to-clipboard :which-key "Copy buffer")

    ;; Help
    "h" '(:ignore t :which-key "Help")
    "h f" '(describe-function :which-key "describe-function")
    "h k" '(describe-key :which-key "describe-key")
    "h c" '(describe-char :which-key "describe-char")
    "h v" '(describe-variable :which-key "describe-variable")
    "h o" '(describe-symbol :which-key "describe-symbol")
    "h m" '(describe-mode :which-key "describe-mode")
    "h F" '(describe-face :which-key "describe-face")
    "h w" '(where-is :which-key "where-is")
    "h ." '(display-local-help :which-key "display-local-help")

    ;; Terminal
    "t" '(vterm :which-key "Terminal")

    ;; Git
    "g" '(:ignore t :which-key "Magit")
    "g g" '(magit-status :which-key "Git status")

    ;; Quit
    "q q" '(save-buffers-kill-terminal :which-key "Quit Emacs")
    "q r" '(restart-emacs :which-key "Restart Emacs")

    ;; Forester
    "r" '(:ignore t :which-key "Forester")
    "r t" '(forester-new-and-goto :which-key "new tree")
    "r d" '(forester-goto :which-key "goto tree")
    "r c" '(forester-new-and-transclude-and-goto :which-key "transclude new tree")

    ;; Org/Notes
    "n" '(:ignore t :which-key "Notes")
    "n l" '(org-roam-buffer-toggle :which-key "Toggle Roam buffer")
    "n f" '(org-roam-node-find :which-key "find node")
    "n g" '(org-roam-graph :which-key "graph")
    "n i" '(org-roam-node-insert :which-key "insert node")
    "n c" '(org-roam-capture :which-key "capture")
    "n j" '(org-roam-dailies-capture-today :which-key "dailies capture")
    "n t" '(org-todo-list :which-key "todo list")
    "n a" '(org-agenda :which-key "agenda")
    "n h" '(org-roam-tag-add :which-key "add tag")
    "n o" '(org-agenda-open-link :which-key "open link at point")
    )

  ;; Magit transient escape
  (general-define-key
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one))

  (defvar my-scroll-timer nil)
  (defvar my-scroll-stop-timer nil)

  (defun my-stop-scroll ()
    (when my-scroll-timer
      (cancel-timer my-scroll-timer)
      (setq my-scroll-timer nil))
    (when my-scroll-stop-timer
      (cancel-timer my-scroll-stop-timer)
      (setq my-scroll-stop-timer nil)))

  (defun my-scroll-viewport (direction lines)
    "Scroll viewport while keeping cursor at same screen position"
    (condition-case nil
        (if (eq direction 'down)
            (progn
              (scroll-up-line lines)
              (evil-next-line lines))
          (progn
            (scroll-down-line lines)
            (evil-previous-line lines)))
      (error (my-stop-scroll))))

  (defun my-continuous-scroll (direction lines)
    (when my-scroll-timer (cancel-timer my-scroll-timer))
    (when my-scroll-stop-timer (cancel-timer my-scroll-stop-timer))

    (setq my-scroll-timer
          (run-with-timer 0 0.01
            (lambda ()
              (my-scroll-viewport direction lines))))

    (setq my-scroll-stop-timer
          (run-with-idle-timer 0.1 nil 'my-stop-scroll)))

  (defun my-scroll-down-continuous ()
    (interactive)
    (my-continuous-scroll 'down 1))

  (defun my-scroll-up-continuous ()
    (interactive)
    (my-continuous-scroll 'up 1))

  (evil-define-key 'normal 'global
    (kbd "M-j") 'my-scroll-down-continuous
    (kbd "<next>") 'my-scroll-down-continuous
    (kbd "M-k") 'my-scroll-up-continuous
    (kbd "<prior>") 'my-scroll-up-continuous)
;;; keybinds.el ends here
