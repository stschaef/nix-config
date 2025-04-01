;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Steven Schaefer"
      user-mail-address "stschaef@umich.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
(setq doom-font (font-spec :family  "JetBrains Mono" :size 16 :weight 'medium))
(setq doom-symbol-font (font-spec :family  "JetBrains Mono" :size 16 :weight 'medium))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-challenger-deep)
(setq doom-theme 'doom-challenger-deep)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-file-format "%m-%d-%Y.org")

(defun db/get-all-directories (directory)
  "Return a list of DIRECTORY and all its subdirectories, excluding directories with a '.orgexclude' file."
  (let ((directories '()))
    (dolist (file (directory-files directory t))
      (when (and (file-directory-p file)
                 (not (string-prefix-p "." (file-name-nondirectory file)))
                 (not (file-exists-p (expand-file-name ".orgexclude" file))))
        (setq directories (append directories (list file)))))
    (append (list directory) (mapcan 'db/get-all-directories directories))))

(defun db/get-org-files-in-directories (directories)
  "Return a list of all .org and .org.gpg files within the given DIRECTORIES."
  (let ((org-files '()))
    (dolist (dir directories)
      (dolist (file (directory-files dir t))
        (let ((name (file-name-nondirectory file)))
          (when (or (and (not (string-prefix-p "." name)) (string-suffix-p ".org" name))
                    (and (not (string-prefix-p "." name)) (string-suffix-p ".org.gpg" name))
                    )
            (push file org-files)))))
    org-files))

(defun db/org-agenda-files ()
  (db/get-org-files-in-directories (db/get-all-directories org-directory)))

(after! org
  (setq org-agenda-files (db/org-agenda-files))
  )

(setq org-journal-enable-agenda-integration t)
(setq exec-path (append exec-path '("/Users/stevenschaefer/.nvm.versions/node/v21.7.0/bin")))

(after! org (setq org-startup-with-latex-preview t))

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)) ; auto-enables fragtog for org-mode)
(setq framemove-hook-into-windmove t)
(framemove-default-keybindings)

(add-hook 'tex-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)

(evil-visual-mark-mode 1)
