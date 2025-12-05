(defvar nix-config-root "/Users/stevenschaefer/nix-config"
  "Root directory of the Nix configuration.")

(defvar nix-config-emacs-d-dir (expand-file-name "emacs/.emacs.d" nix-config-root)
  "Emacs configuration directory within nix-config.")

(defvar nix-config-emacs-keybinds (expand-file-name "keybinds.el" nix-config-emacs-d-dir)
  "Path to keybinds.el file.")

(defun nix-rebuild ()
  "Run system rebuild command based on the operating system."
  (interactive)
  (let* ((rebuild-cmd (cond
                       ((eq system-type 'darwin) "darwin-rebuild switch")
                       ((eq system-type 'gnu/linux) "nixos-rebuild switch")
                       (t (error "Unsupported system type"))))
         (default-directory "/sudo::"))
    (async-shell-command rebuild-cmd)))
