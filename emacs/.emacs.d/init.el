;;; init.el --- Main configuration

;;; ============================================================
;;; PHASE 1: Package System Setup (NO NETWORK)
;;; ============================================================

(require 'package)

;; Add package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize package system (loads local package metadata only, no network)
(package-initialize)

;; Set custom emacs directory
(setq user-emacs-directory "~/nix-config/emacs/.emacs.d/")

;; Set custom-file to write to your nix-config directory
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; ============================================================
;;; PHASE 2: Install Packages (SYNCHRONOUS)
;;; ============================================================

(message "Installing packages...")
(condition-case err
    (progn
      ;; Refresh package contents on first run or if packages are missing
      (unless (and (package-installed-p 'use-package)
                   (file-exists-p (expand-file-name "elpa" user-emacs-directory)))
        (package-refresh-contents))

      ;; Install use-package if needed
      (unless (package-installed-p 'use-package)
        (message "Installing use-package...")
        (package-install 'use-package))

      ;; Load and configure use-package
      (require 'use-package)
      (setq use-package-always-ensure t)

      ;; Install all selected packages
      (message "Installing selected packages...")
      (package-install-selected-packages 'no-confirm)
      (message "All packages installed"))
  (error
   (message "Package installation failed: %s"
            (error-message-string err))))

;;; ============================================================
;;; PHASE 3: Load All Your Configuration Files
;;; ============================================================

(defvar my-config-priorities
  '(
    ("org.el" . 99)
    ("agda-setup.el" . 9)
    ("custom.el" . 99)
    ("email.el" . 99)
    ("evil.el" . 5)
    ("keybinds.el" . 6)
    ("nix.el" . 5)
    ("packages.el" . 2)
    ("search-and-completion.el" . 10)
    ("tex.el" . 99)
    ("ui.el" . 2)
    ("utils.el" . 10))
  "Alist of file patterns and their load priorities.")

(defun my-get-file-priority (filename)
  "Get the priority for FILENAME based on my-config-priorities."
  (or (cdr (assoc filename my-config-priorities))
      (cl-loop for (pattern . priority) in my-config-priorities
               when (string-match-p pattern filename)
               return priority)
      50)) ; Default priority

(let* ((all-files (directory-files user-emacs-directory t "\\`[^.].*\\.el\\'"))
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
    (message "Loading %s..." (file-name-nondirectory file))
    (condition-case err
        (load file)
      (error
       (message "ERROR loading %s: %s"
                (file-name-nondirectory file)
                (error-message-string err))))))

(setq warning-suppress-types '((lsp-mode)))

;; Load custom-file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

(message "All configuration files loaded. Emacs is ready!")

;;; ============================================================
;;; PHASE 4: Background Package Updates (ASYNC, OPTIONAL)
;;; ============================================================

;; Optional: Check for package updates in the background
(run-with-idle-timer
 5
 nil
 (lambda ()
   (message "[Background] Checking for package updates...")
   (condition-case err
       (progn
         (package-refresh-contents)
         (message "[Background] Package update check complete"))
     (error
      (message "[Background] Package update check failed: %s"
               (error-message-string err))))))

;;; init.el ends here
