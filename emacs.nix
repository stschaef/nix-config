# emacs.nix
{ config, pkgs, lib, emacsPackage ? pkgs.emacs, ... }:

{
  programs.emacs = {
    enable = true;
    package = emacsPackage;

    # Install all your required packages through Nix
    extraPackages = epkgs: with epkgs; [
      # Core packages
      use-package
      # org
      # evil
      # which-key
      # magit
      # evil-collection
      # auctex
      # flycheck
      # ivy
      # smex
      # counsel
      # ivy-rich
      # dashboard
      # projectile
      # org-journal
      # company
      # quelpa
      # quelpa-use-package
      # undo-tree
      # org-bullets
      doom-themes
      # general

      # Additional packages from your config
      # f
      # editorconfig
      # s
      # nerd-icons

      # You can add more packages here
    ];

    # Your init.el content as a string
  #   extraConfig = ''
  #     ;; Your entire init.el content goes here
  #     (require 'package)
  #     (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
  #     (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  #     (package-initialize)
  #
  #     (unless (package-installed-p 'use-package)
  #       (package-refresh-contents)
  #       (package-install 'use-package))
  #     (eval-and-compile
  #       (setq use-package-always-ensure t
  #             use-package-expand-minimally t))
  #
  #     ;; Your use-package declarations and other configuration...
  #     (use-package org)
  #     (use-package org-agenda
  #       :ensure org)
  #     (use-package evil
  #      :ensure t
  #      :init
  #        (setq evil-want-keybinding nil)
  #        (setq evil-search-module 'evil-search)
  #        (setq evil-vsplit-window-right t)
  #        (setq evil-split-window-below t)
  #        (setq evil-move-cursor-back nil)
  #      :config
  #        (evil-mode 1)
  #      )
  #
  #     ;; Rest of your configuration...
  #
  #     ;; Set theme
  #     (use-package doom-themes
  #       :ensure t
  #       :config
  #       (load-theme 'doom-challenger-deep t))
  #
  #     (set-frame-font "JetBrains Mono-14:weight=medium" nil t)
  #   '';
  # };
  #
  # # Handle Copilot separately since it's not in standard repositories
  # home.file.".emacs.d/copilot" = {
  #   source = pkgs.fetchFromGitHub {
  #     owner = "copilot-emacs";
  #     repo = "copilot.el";
  #     rev = "main"; # You might want to pin to a specific commit
  #     sha256 = ""; # You'll need to get the correct hash
  #   };
  #   recursive = true;
  # };
  #
  # # Set up Agda mode
  # home.file.".emacs.d/agda-mode.el" = {
  #   text = ''
  #     (load-file (let ((coding-system-for-read 'utf-8))
  #                     (shell-command-to-string "agda-mode locate")))
  #   '';
  };
  #
  # # Ensure org directories exist
  # home.file."org/journal/.keep".text = "";
}

