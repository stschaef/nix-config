{ config, pkgs, lib,... }:

{
  # Basic system settings
  nixpkgs.hostPlatform = "aarch64-darwin"; # or x86_64-darwin for Intel

  # Auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  # touchID for sudo
  security.pam.enableSudoTouchIdAuth = true;

  environment.systemPackages = import ./packages.nix { inherit pkgs; };

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
}
