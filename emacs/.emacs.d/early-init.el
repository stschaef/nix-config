;;; early-init.el --- Early initialization for maximum performance -*- lexical-binding: t; -*-

;;; ============================================================
;;; CRITICAL: These settings run BEFORE the package system loads
;;; ============================================================

;; Increase GC threshold during startup (restore later in init.el)
;; This is the single biggest startup optimization
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Prevent package.el from initializing before we're ready
(setq package-enable-at-startup nil)

;; Disable expensive file-name-handler operations during startup
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist)))

;;; ============================================================
;;; UI ELEMENTS - Disable BEFORE frame is drawn (prevents flicker)
;;; ============================================================

;; Disable UI elements early to prevent momentary display
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)))

;; Explicitly disable modes (belt and suspenders)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 1) default-frame-alist)  ; Keep menu bar per your preference

;; Prevent the glimpse of un-styled Emacs
(setq inhibit-redisplay t)
(setq inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq inhibit-redisplay nil)
            (setq inhibit-message nil)
            (redisplay)))

;;; ============================================================
;;; EVIL SETUP - Must be set before evil or evil-collection loads
;;; ============================================================

;; Required for evil-collection - must be set before evil loads
(setq evil-want-keybinding nil)

;;; ============================================================
;;; NETWORK & GPG - Fix hanging issues
;;; ============================================================

;; CRITICAL: Disable GPG signature verification to prevent hangs
;; This is safe for packages from trusted archives (MELPA, ELPA)
(setq package-check-signature nil)

;; Increase network timeouts
(setq url-queue-timeout 30)
(setq url-http-attempt-keepalives nil)

;; Set GPG program explicitly (prevents hanging on keyring import)
(when (eq system-type 'darwin)
  (setq epg-gpg-program "/opt/homebrew/bin/gpg"))

;; Prevent GPG from prompting for passphrase in minibuffer
(setq epg-pinentry-mode 'loopback)

;;; ============================================================
;;; NATIVE COMPILATION (Emacs 28+)
;;; ============================================================

(when (featurep 'native-compile)
  ;; Silence native-comp warnings
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Set native-comp cache directory
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache
     (expand-file-name "eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
