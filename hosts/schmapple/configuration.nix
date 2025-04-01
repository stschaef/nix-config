{ config, pkgs, lib,... }:

{
  # Basic system settings
  nixpkgs.hostPlatform = "aarch64-darwin"; # or x86_64-darwin for Intel

  environment.systemPackages = import ./packages.nix { inherit pkgs; };

  # Enable Homebrew and install packages
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
    };

    taps = [
      "d12frosted/emacs-plus"
    ];

    brews = [
      "emacs-plus@30"
      "switchaudio-osx"
      "nowplaying-cli"
    ];

    casks = [
      "sf-symbols"
      "font-sf-mono"
      "font-sf-pro"
    ];
  };

  # Used for backwards compatibility
  system.stateVersion = 4;
}
