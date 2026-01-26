;;; email.el --- Email configuration with mu4e -*- lexical-binding: t; -*-

;;; ============================================================
;;; mu4e Configuration
;;; ============================================================

;; Set mu4e load path based on OS
(defvar mu4e-load-path
  (cond
   ((eq system-type 'darwin) "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/")
   ((eq system-type 'gnu/linux) "/usr/share/emacs/site-lisp/mu4e/")
   (t nil))
  "Path to mu4e installation.")

(when (and mu4e-load-path (file-exists-p mu4e-load-path))
  (add-to-list 'load-path mu4e-load-path))

(use-package mu4e
  :ensure nil
  :commands (mu4e mu4e-headers-search mu4e-compose-new)
  :config
  (require 'smtpmail)

  ;; Basic settings
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval 300)
  (setq mu4e-attachment-dir "~/Desktop/Attachments")
  (setq mu4e-change-filenames-when-moving t)

  ;; Maildir shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/Gmail/Inbox" . ?i)
          ("/Gmail/[Gmail]/Sent Mail" . ?s)
          ("/Gmail/[Gmail]/Starred" . ?t)
          ("/Umich/Inbox" . ?I)
          ("/Umich/[Gmail]/Sent Mail" . ?S)
          ("/Umich/[Gmail]/Starred" . ?T)))

  ;; Email contexts for multiple accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "gmail"
            :enter-func
            (lambda () (mu4e-message "Entering schaefer.steven.ss@gmail.com context"))
            :leave-func
            (lambda () (mu4e-message "Leaving schaefer.steven.ss@gmail.com context"))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches msg :to "schaefer.steven.ss@gmail.com")))
            :vars '((user-mail-address . "schaefer.steven.ss@gmail.com")
                    (user-full-name . "Steven Schaefer")
                    (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
                    (mu4e-refile-folder . "/Gmail/Archive")
                    (mu4e-sent-folder . "/Gmail/[Gmail]/Sent Mail")
                    (mu4e-trash-folder . "/Gmail/[Gmail]/Trash")))
          ,(make-mu4e-context
            :name "umich"
            :enter-func
            (lambda () (mu4e-message "Entering stschaef@umich.edu context"))
            :leave-func
            (lambda () (mu4e-message "Leaving stschaef@umich.edu context"))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches msg :to "stschaef@umich.edu")))
            :vars '((user-mail-address . "stschaef@umich.edu")
                    (user-full-name . "Steven Schaefer")
                    (mu4e-drafts-folder . "/Umich/[Gmail]/Drafts")
                    (mu4e-refile-folder . "/Umich/Archive")
                    (mu4e-sent-folder . "/Umich/[Gmail]/Sent Mail")
                    (mu4e-trash-folder . "/Umich/[Gmail]/Trash")))))

  ;; Context policy
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)

  ;; Message settings
  (setq message-kill-buffer-on-exit t)

  ;; SMTP configuration using msmtp
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (executable-find "msmtp")
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header)

  ;; Default to gmail account for sending
  (setq message-sendmail-extra-arguments '("-a" "gmail"))

  ;; Auto add Cc and Bcc headers
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (save-excursion (message-add-header "Cc:\n"))
              (save-excursion (message-add-header "Bcc:\n"))))

  ;; Address completion
  (add-hook 'mu4e-compose-mode-hook 'company-mode)

  ;; UI preferences
  (setq mu4e-confirm-quit nil)
  (setq mu4e-headers-visible-lines 20)
  (setq mu4e-headers-show-threads nil)
  (setq mu4e-hide-index-messages t)
  (setq mu4e-headers-include-related nil)

  ;; Citation format
  (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)

  ;; Store link to message if in header view
  (setq org-mu4e-link-query-in-headers-mode nil)

  ;; Evil integration
  (with-eval-after-load 'evil
    (evil-set-initial-state 'mu4e-main-mode 'normal)
    (evil-set-initial-state 'mu4e-headers-mode 'normal)
    (evil-set-initial-state 'mu4e-view-mode 'normal)))

;; mu4e-alert for notifications
(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'notifier))
