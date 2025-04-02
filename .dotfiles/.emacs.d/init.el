;;; Initialize package system
(require 'package)

;; Add package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize
(package-initialize)

;;; Initialize package system
(require 'package)

;; Add package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Refresh package list
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Always ensure packages are installed
(setq use-package-always-ensure t)

(use-package org-agenda
 :ensure org)

(use-package evil
 :ensure t
 :init
   (setq evil-want-keybinding nil)
   (setq evil-search-module 'evil-search)
   (setq evil-vsplit-window-right t)
   (setq evil-split-window-below t)
   (setq evil-move-cursor-back nil)
 :config
   (evil-mode 1)
)
(use-package evil-surround
  :ensure
  :config
  (global-evil-surround-mode 1))
(use-package vimish-fold
  :ensure
  :after evil
)
(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))
(use-package evil-visual-mark-mode
  :ensure t
  :init (evil-visual-mark-mode 1))
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))
;; TODO look into evil embrace

(use-package auctex)

(use-package flycheck)

(use-package consult :ensure t)

(setq mac-command-modifier 'meta)

(use-package magit
  :ensure t
  :config
  (define-key magit-status-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
  (define-key magit-revision-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
)

(use-package evil-collection
  :ensure t
  :after (evil magit)
  :config
 (evil-collection-init 'magit)) ; This initializes the evil bindings for magit

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode 1)
 )
(evil-set-undo-system 'undo-redo)

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

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-switch-function 'projectile-switch-project-by-name)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-sort-function
        (lambda (projects)
          (let ((recent-projects-list (projectile-relevant-known-projects)))
            (if recent-projects-list
                ;; Return only the most recent ones (limited by dashboard-items setting)
                (seq-take recent-projects-list
                          (cdr (assoc 'projects dashboard-items)))
              projects))))
  (setq dashboard-items '((recents . 10)
                          (projects . 10)
                          (bookmarks . 10)
			  ))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-challenger-deep t))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-auto-revert-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;(set-frame-font "JetBrains Mono-14:weight=medium" nil t)


(use-package company
  :ensure t
  :config (global-company-mode 1))
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package hl-todo :ensure t :init (global-hl-todo-mode 1))
(use-package unicode-fonts :ensure t)

(defun kill-other-buffers ()
     "Kill all other buffers."
     (interactive)
     (mapc #'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package nix-mode
  :mode "\\.nix\\'")

(defun move-this-file (new-path)
  "Move current buffer's file to NEW-PATH."
  (interactive
   (list (read-file-name "Move file to: ")))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path 1)
    (set-visited-file-name new-path t t)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(defun copy-this-file (new-path)
  "Copy current buffer's file to NEW-PATH then open NEW-PATH."
  (interactive
   (list (read-file-name "Copy file to: ")))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path 1)
    (find-file new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

(defun delete-this-file (&optional path)
  "Delete PATH, kill its buffers.
If PATH is not specified, default to the current buffer's file."
  (interactive
   (list (buffer-file-name)))
  (let* ((path (or path (buffer-file-name)))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (y-or-n-p (format "Really delete %S?" short-path))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; Kill all buffers visiting this file
          (dolist (buffer (buffer-list))
            (when (string= (buffer-file-name buffer) path)
              (kill-buffer buffer)))
          (message "Deleted %S" short-path))))))


;; Define a function to copy entire buffer to clipboard
(defun copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard"))

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
    ;"ec" '(consult-theme :which-key "consult-theme")
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

    ;; tabs
    ;; "TAB" '(:ignore t :which-key "Tabs")
    ;; "TAB TAB" '(tab-bar-switch-to-tab :which-key "tab-bar-switch-to-tab")
    ;; "TAB [" '(+tab-bar/switch-to-prev-tab :which-key "+tab-bar/switch-to-prev-tab")
    ;; "TAB ]" '(+tab-bar/switch-to-next-tab :which-key "+tab-bar/switch-to-next-tab")
    ;; "TAB n" '(+tab-bar/add-new :which-key "+tab-bar/add-new")
    ;; "TAB k" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
    ;; "TAB d" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
    ;; "TAB K" '(+tab-bar/close-all-tabs-except-current :which-key "+tab-bar/close-all-tabs-except-current")
    ;; "TAB r" '(tab-rename :which-key "tab-rename")

    ;; quick tab switching
    "1" '((lambda () (interactive) (+tab-bar/switch-by-index 0)) :which-key nil)
    "2" '((lambda () (interactive) (+tab-bar/switch-by-index 1)) :which-key nil)
    "3" '((lambda () (interactive) (+tab-bar/switch-by-index 2)) :which-key nil)
    "4" '((lambda () (interactive) (+tab-bar/switch-by-index 3)) :which-key nil)
    "5" '((lambda () (interactive) (+tab-bar/switch-by-index 4)) :which-key nil)
    "6" '((lambda () (interactive) (+tab-bar/switch-by-index 5)) :which-key nil)
    "7" '((lambda () (interactive) (+tab-bar/switch-by-index 6)) :which-key nil)
    "8" '((lambda () (interactive) (+tab-bar/switch-by-index 7)) :which-key nil)
    "9" '((lambda () (interactive) (+tab-bar/switch-by-index 8)) :which-key nil)

    ;; git
    "g" '(:ignore t :which-key "Git") ; prefix
    "gg" '(magit-status :which-key "Git status")

    "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
    "qr" '(restart-emacs :which-key "Restart Emacs")
    )

  ;; magit
  (general-define-key
    ;; https://github.com/emacs-evil/evil-magit/issues/14#issuecomment-626583736
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one)
)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(defun setup-agda2-evil-keybindings ()
  "Configure keybindings for working with Agda in Evil mode."
  (evil-define-key 'normal agda2-mode-map
    "gd" 'agda2-goto-definition-keyboard))

(add-hook 'agda2-mode-hook #'setup-agda2-evil-keybindings)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
