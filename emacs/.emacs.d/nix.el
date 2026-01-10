(defvar nix-config-root "/Users/stevenschaefer/nix-config"
  "Root directory of the Nix configuration.")

(defvar nix-config-emacs-d-dir (expand-file-name "emacs/.emacs.d" nix-config-root)
  "Emacs configuration directory within nix-config.")

(defvar nix-config-flake (expand-file-name "flake.nix" nix-config-root)
  "Path to flake.nix file.")

(defvar nix-config-packages (expand-file-name "packages.nix" nix-config-root)
  "Path to packages.nix file.")

(defvar nix-config-emacs-keybinds (expand-file-name "keybinds.el" nix-config-emacs-d-dir)
  "Path to keybinds.el file.")

(defvar nix-config-emacs-packages (expand-file-name "packages.el" nix-config-emacs-d-dir)
  "Path to packages.el file.")

(defvar nix-config-emacs-init (expand-file-name "init.el" nix-config-emacs-d-dir)
  "Path to init.el file.")

(defun nix-rebuild ()
  "Run system rebuild command based on the operating system."
  (interactive)
  (let ((rebuild-cmd (cond
                      ((eq system-type 'darwin) "sudo darwin-rebuild switch")
                      ((eq system-type 'gnu/linux) "sudo nixos-rebuild switch")
                      (t (error "Unsupported system type")))))
    (async-shell-command rebuild-cmd)))
