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

;;; ============================================================
;;; PHASE 2: Bootstrap use-package (if already installed)
;;; ============================================================

(when (package-installed-p 'use-package)
  (require 'use-package)
  (setq use-package-always-ensure t))

;;; ============================================================
;;; PHASE 3: Load All Your Configuration Files (NO NETWORK)
;;; ============================================================

(message "Loading configuration files...")
(dolist (file (directory-files user-emacs-directory t "\\`[^.].*\\.el\\'"))
  (when (and (not (string-match-p "init\\.el$" file))
             (not (string-match-p "early-init\\.el$" file))
             (not (string-match-p "elpa" file))
             (not (and (eq system-type 'gnu/linux)
                       (string-match-p "email\\.el$" file)))
             (not (and (eq system-type 'gnu/linux)
                       (string-match-p "tex\\.el$" file)))
             (not (string-match-p "custom\\.el$" file)))
    (message "  Loading %s..." (file-name-nondirectory file))
    (condition-case err
        (load file)
      (error
       (message "  ERROR loading %s: %s"
                (file-name-nondirectory file)
                (error-message-string err))))))

(message "All configuration files loaded. Emacs is ready!")

(setq warning-suppress-types '((lsp-mode)))

;;; ============================================================
;;; PHASE 4: Deferred Network Operations (ASYNC, NON-BLOCKING)
;;; ============================================================

(run-with-idle-timer
 2
 nil
 (lambda ()
   (message "[Background] Starting package operations...")
   (condition-case err
       (progn
         (message "[Background] Refreshing package contents...")
         (package-refresh-contents)
         (message "[Background] Package refresh complete")

         (unless (package-installed-p 'use-package)
           (message "[Background] Installing use-package...")
           (package-install 'use-package)
           (require 'use-package)
           (setq use-package-always-ensure t)
           (message "[Background] use-package installed"))

         (message "[Background] Installing selected packages...")
         (package-install-selected-packages 'no-confirm)
         (message "[Background] All packages installed")

         (message "[Background] All package operations complete"))
     (error
      (message "[Background] Package operation failed: %s"
               (error-message-string err))))))

;; Set custom emacs directory
(setq user-emacs-directory "~/nix-config/emacs/.emacs.d/")

;; Set custom-file to write to your nix-config directory
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load custom-file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
