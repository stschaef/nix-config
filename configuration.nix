{ config, pkgs, inputs, ... }:

{
  imports = [ ./packages.nix ./tex.nix];

  # Enable nix-darwin's own Nix installation management
  nix.enable = true;

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    substituters = [
      "https://cache.nixos.org"
      "https://forester.cachix.org"
    ];
    trusted-public-keys = [
      "forester.cachix.org-1:pErGVVci7kZWxxcbQ/To8Lvqp6nVTeyPf0efJxbrQDM="
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Enable Zsh shell
  programs.zsh.enable = true;

  fonts.packages = with pkgs; [
    nerdfonts
  ];
}
