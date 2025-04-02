{ config, pkgs, ... }:

{
  # Enable nix-darwin's own Nix installation management
  nix.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = import ./packages.nix { inherit pkgs; };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Enable Zsh shell
  programs.zsh.enable = true;

  fonts.packages = with pkgs; [
    nerdfonts
  ];
  fonts.fontconfig.enable = true;
}
