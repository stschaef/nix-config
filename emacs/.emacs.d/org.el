(use-package visual-fill-column
  :ensure t)

(use-package org-present
  :ensure t
  :hook ((org-present-mode . my/org-present-start)
         (org-present-mode-quit . my/org-present-end))
  :init
  (defun my/org-present-start ()
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    (setq visual-fill-column-width 90
          visual-fill-column-center-text t)

    (setq-local face-remapping-alist
                '((default (:height 1.8 :family "JuliaMono") default)
                  (org-document-title (:height 2.0 :weight bold) org-document-title)
                  (org-level-1 (:height 1.8 :weight bold) org-level-1)
                  (org-level-2 (:height 1.6 :weight bold) org-level-2)
                  (org-level-3 (:height 1.4 :weight bold) org-level-3)
                  (org-code (:height 1.4 :family "JuliaMono") org-code)
                  (org-verbatim (:height 1.4 :family "JuliaMono") org-verbatim)
                  (org-block (:height 1.2 :family "JuliaMono") org-block)
                  (org-block-begin-line (:height 0.8) org-block-begin-line)))

    (org-display-inline-images)
    (setq-local cursor-type nil)

    ;; Fix heading alignment - remove leading space after hidden asterisks
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (let ((ov (make-overlay (1- (point)) (point))))
          (overlay-put ov 'invisible t)
          (overlay-put ov 'org-present-heading-space t))))

    ;; Save and hide modeline
    (setq-local my/org-present-modeline-format mode-line-format)
    (setq-local mode-line-format nil)

    (display-line-numbers-mode -1)
    (read-only-mode 1)

    (when (featurep 'evil)
      (evil-define-key 'normal org-present-mode-keymap
        (kbd "h") 'org-present-prev
        (kbd "l") 'org-present-next
        (kbd "k") 'org-present-prev
        (kbd "j") 'org-present-next
        (kbd "q") 'org-present-quit)))

  (defun my/org-present-end ()
    (visual-fill-column-mode 0)
    (visual-line-mode 0)
    (display-line-numbers-mode 1)
    (setq-local face-remapping-alist nil)
    (setq-local cursor-type t)

    ;; Remove our custom overlays
    (remove-overlays (point-min) (point-max) 'org-present-heading-space t)

    ;; Restore modeline
    (setq-local mode-line-format my/org-present-modeline-format)

    (read-only-mode -1)))

(use-package org-roam
  :ensure t
  :init
  ;; Acknowledge that you're using Org Roam v2
  (setq org-roam-v2-ack t)
  :custom
  ;; Set the directory where your notes will be stored
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  :config
  ;; Initialize the database and setup Org Roam
  (org-roam-setup))
