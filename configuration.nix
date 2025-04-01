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
}
