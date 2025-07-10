{ config, pkgs, lib,... }:

{
  # Basic system settings
  nixpkgs.hostPlatform = "aarch64-darwin"; # or x86_64-darwin for Intel


  # touchID for sudo
  security.pam.services.sudo_local.touchIdAuth = true;

  environment.systemPackages = import ./packages.nix { inherit pkgs; };

  nix.settings.trusted-users = [ "stevenschaefer" ];

  # Enable Homebrew and install packages
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
    };

    brews = [
      "mu"
      "cowsay"
    ];
  };

  # Used for backwards compatibility
  system.stateVersion = 4;
  system.primaryUser = "stevenschaefer";

  ids.gids.nixbld = 350;
}
