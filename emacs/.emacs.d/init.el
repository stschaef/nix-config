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

;; Load all .el files in the same directory as init.el
(dolist (file (directory-files user-emacs-directory t "\\`[^.].*\\.el\\'"))
  (when (and (not (string-match-p "init\\.el$" file))
             (not (string-match-p "elpa" file))
	     (not (or (and (string-equal system-type 'gnu/linux)
			   (string-match-p "email\\.el$" file))))
	     (not (or (and (string-equal system-type 'gnu/linux)
			   (string-match-p "tex\\.el$" file))))
             (not (string-match-p "custom\\.el$" file)))
    (load file)))
