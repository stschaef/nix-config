(defun kill-other-buffers ()
     "Kill all other buffers."
     (interactive)
     (mapc #'kill-buffer (delq (current-buffer) (buffer-list))))

(defun move-this-file (new-path)
  "Move current buffer's file to NEW-PATH."
  (interactive
   (list (read-file-name "Move file to: ")))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path 1)
    (set-visited-file-name new-path t t)
    (message "File moved to %S" (abbreviate-file-name new-path)))))

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
