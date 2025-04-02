{ config, pkgs, inputs, ... }:

{
  imports = [ ./packages.nix ];
  # Enable nix-darwin's own Nix installation management
  nix.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;


  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Enable Zsh shell
  programs.zsh.enable = true;

  fonts.packages = with pkgs; [
    nerdfonts
  ];
}
