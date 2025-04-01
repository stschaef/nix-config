{ config, pkgs, ... }:

{
  # Enable nix-darwin's own Nix installation management
  nix.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  environment.systemPackages = import ./packages.nix { inherit pkgs; };

  # Enable Zsh shell
  programs.zsh.enable = true;

  # programs.emacs = {
  #   enable = true;
  #   source = ./.dotfiles/emacs/init.el;
  #   extraPackages = epkgs: with epkgs; [
  #     # Core packages
  #     use-package
  #     # org
  #     # evil
  #     # which-key
  #     # magit
  #     # evil-collection
  #     # auctex
  #     # flycheck
  #     # ivy
  #     # smex
  #     # counsel
  #     # ivy-rich
  #     # dashboard
  #     # projectile
  #     # org-journal
  #     # company
  #     # quelpa
  #     # quelpa-use-package
  #     # undo-tree
  #     # org-bullets
  #     doom-themes
  #     # general
  #
  #     # Additional packages from your config
  #     # f
  #     # editorconfig
  #     # s
  #     # nerd-icons
  #
  #     # You can add more packages here
  #   ];
  # };
}
