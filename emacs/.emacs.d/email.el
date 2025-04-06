(cond
 ;; macOS specific configuration
 ((eq system-type 'darwin)
  (setq mu4e-load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/")
 )

 ;; Linux specific configuration
 ((eq system-type 'gnu/linux)
  (message "Need to implement mu4e on linux")
 ))

(use-package mu4e
  :ensure nil
  :load-path mu4e-load-path
)

(require 'smtpmail)

(setq mu4e-mu-binary (executable-find "mu"))

(setq mu4e-maildir "~/Mail")

(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-update-interval 300)
(setq mu4e-attachment-dir "~/Desktop/Attachments")
(setq mu4e-change-filenames-when-moving t)

(setq   mu4e-maildir-shortcuts
        '(("/Gmail/Inbox" . ?i)
          ("/Gmail/[Gmail]/Sent Mail" . ?s)
          ("/Gmail/[Gmail]/Starred" . ?t)
          ("/Umich/Inbox" . ?I)
          ("/Umich/[Gmail]/Sent Mail" . ?S)
          ("/Umich/[Gmail]/Starred" . ?T)
	  ))
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "gmail"
          :enter-func
          (lambda () (mu4e-message "Entering schaefer.steven.ss@gmail.com context"))
          :leave-func
          (lambda () (mu4e-message "Leaving schaefer.steven.ss@gmail.com context"))
          :match-func
          (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "schaefer.steven.ss@gmail.com")))
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
          (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "stschaef@umich.edu")))
          :vars '((user-mail-address . "stschaef@umich.edu")
                  (user-full-name . "Steven Schaefer")
                  (mu4e-drafts-folder . "/Umich/[Gmail]/Drafts")
                  (mu4e-refile-folder . "/Umich/Archive")
                  (mu4e-sent-folder . "/Umich/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/Umich/[Gmail]/Trash")))
	))


(setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;

(setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;
;; gpg encryptiom & decryption:

;; this can be left alone

(require 'epa-file)

(epa-file-enable)

(setq epa-pinentry-mode 'loopback)

(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:

(setq message-kill-buffer-on-exit t)

;; (setq sendmail-program (executable-find "msmtp"))
;; (setq sendmail-function 'message-send-mail-with-sendmail)
;; (setq message-sendmail-envelope-from 'header)

;; With these lines
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program (executable-find "msmtp")
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

;; Add this line to pass the account explicitly
(setq message-sendmail-extra-arguments '("-a" "gmail"))

(add-hook 'mu4e-compose-mode-hook

          (defun timu/add-cc-and-bcc ()

            "My Function to automatically add Cc & Bcc: headers.

    This is in the mu4e compose mode."

            (save-excursion (message-add-header "Cc:\n"))

            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)

;; store link to message if in header view, not to header query:
(setq org-mu4e-link-query-in-headers-mode nil)

;; don't have to confirm when quitting:
(setq mu4e-confirm-quit nil)

;; number of visible headers in horizontal split view:
(setq mu4e-headers-visible-lines 20)

;; don't show threading by default:
(setq mu4e-headers-show-threads nil)

;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
(setq mu4e-hide-index-messages t)

;; customize the reply-quote-string:
(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")

;; M-x find-function RET message-citation-line-format for docs:
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; by default do not show related emails:

(setq mu4e-headers-include-related nil)

;; by default do not show threads:

(setq mu4e-headers-show-threads nil)

(use-package mu4e-alert)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
