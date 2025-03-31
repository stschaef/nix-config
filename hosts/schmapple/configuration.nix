{ config, pkgs, ... }:

{
  # Basic system settings
  nixpkgs.hostPlatform = "aarch64-darwin"; # or x86_64-darwin for Intel

  # Enable nix-darwin's own Nix installation management
  nix.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  # Create /etc/zshrc that loads the nix-darwin environment
  programs.zsh.enable = true;

  environment.systemPackages = import ./packages.nix { inherit pkgs; };

  # Used for backwards compatibility
  system.stateVersion = 4;
}
