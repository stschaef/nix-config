;;; init.el --- Main configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; PHASE 1: Package System Setup (NO NETWORK AT STARTUP)
;;; ============================================================

(require 'package)

;; Add package repositories with priorities (prefer faster/reliable ones)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Prioritize faster/more reliable archives
(setq package-archive-priorities
      '(("melpa" . 3)
        ("elpa" . 2)
        ("org" . 1)
        ("nongnu" . 0)))

;; Initialize package system (loads LOCAL metadata only, NO NETWORK)
(package-initialize)

;; Keep user-emacs-directory as ~/.emacs.d for transient files
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

;; Set custom-file to write to your nix-config directory ONLY
(setq custom-file (expand-file-name "~/nix-config/emacs/.emacs.d/custom.el"))

;; Explicitly set transient file locations to ~/.emacs.d
(setq savehist-file (expand-file-name "history" user-emacs-directory))
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
(setq recentf-save-file
      (expand-file-name "recentf" user-emacs-directory))
(setq transient-history-file
      (expand-file-name "transient/history.el" user-emacs-directory))
(setq transient-levels-file
      (expand-file-name "transient/levels.el" user-emacs-directory))
(setq transient-values-file
      (expand-file-name "transient/values.el" user-emacs-directory))

;;; ============================================================
;;; PHASE 2: Setup use-package (NO NETWORK REFRESH)
;;; ============================================================

;; Install use-package ONLY if not already installed
;; This should only happen on first run
(unless (package-installed-p 'use-package)
  (message "First run: installing use-package...")
  (package-refresh-contents)
  (package-install 'use-package))

;; Load and configure use-package
(eval-when-compile
  (require 'use-package))

;; Configure use-package for lazy loading by default
(setq use-package-always-ensure t)
(setq use-package-always-defer t)  ;; CRITICAL: lazy load everything by default
(setq use-package-expand-minimally t)  ;; Faster macro expansion
(setq use-package-verbose nil)  ;; Silence output

;;; ============================================================
;;; PHASE 3: Load Configuration Files (with timing)
;;; ============================================================

(defvar my-config-priorities
  '(("ui.el" . 1)           ;; UI first for visual feedback
    ("packages.el" . 2)
    ("nix.el" . 3)
    ("evil.el" . 4)         ;; Evil early for keybindings
    ("keybinds.el" . 5)
    ("utils.el" . 6)
    ("agda-setup.el" . 10)
    ("search-and-completion.el" . 10)
    ("agents.el" . 11)
    ("org.el" . 20)         ;; Org later (can be deferred)
    ("tex.el" . 20)
    ("custom.el" . 99))
  "Alist of file patterns and their load priorities.")

(defun my-get-file-priority (filename)
  "Get the priority for FILENAME based on my-config-priorities."
  (or (cdr (assoc filename my-config-priorities))
      (cl-loop for (pattern . priority) in my-config-priorities
               when (string-match-p pattern filename)
               return priority)
      50))

;; Load configuration files
(let* ((config-dir (expand-file-name "~/nix-config/emacs/.emacs.d/"))
       (all-files (directory-files config-dir t "\\`[^.].*\\.el\\'"))
       (config-files (cl-remove-if
                      (lambda (f)
                        (or (string-match-p "init\\.el$" f)
                            (string-match-p "early-init\\.el$" f)
                            (string-match-p "custom\\.el$" f)
                            (string-match-p "elpa" f)))
                      all-files))
       (sorted-files (sort config-files
                          (lambda (a b)
                            (< (my-get-file-priority (file-name-nondirectory a))
                               (my-get-file-priority (file-name-nondirectory b)))))))
  (dolist (file sorted-files)
    (condition-case err
        (load file nil t)  ;; nil = no message, t = no suffix
      (error
       (message "ERROR loading %s: %s"
                (file-name-nondirectory file)
                (error-message-string err))))))

;; Load custom-file if it exists
(when (file-exists-p custom-file)
  (load custom-file nil t))

;; Suppress LSP warnings
(setq warning-suppress-types '((lsp-mode)))

;;; ============================================================
;;; PHASE 4: Restore GC and Finalize
;;; ============================================================

;; Restore GC threshold to reasonable value after startup
;; 16MB is good for modern machines, prevents thrashing
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq gc-cons-percentage 0.1)
            (message "Emacs ready in %.2f seconds with %d GCs"
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;;; ============================================================
;;; PHASE 5: Manual Package Refresh Command
;;; ============================================================

;; Provide a command to manually refresh packages when needed
(defun my/package-refresh ()
  "Manually refresh package contents. Use this instead of automatic refresh."
  (interactive)
  (message "Refreshing package contents...")
  (package-refresh-contents)
  (message "Package refresh complete."))

;;; init.el ends here
