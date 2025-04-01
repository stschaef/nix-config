{ config, pkgs, lib,... }:

{
  nixpkgs.overlays = [
    (final: prev: {
      sbar-lua = prev.callPackage ./sketchybar/sbarlua.nix {};
    })
  ];
  # Basic system settings
  nixpkgs.hostPlatform = "aarch64-darwin"; # or x86_64-darwin for Intel

  # Create /etc/zshrc that loads the nix-darwin environment
  programs.zsh.enable = true;

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

  services.sketchybar = {
    enable = true;
    config = ''
      sketchybar --bar height=24
      sketchybar update
    '';
  };
  # Used for backwards compatibility
  system.stateVersion = 4;
}
