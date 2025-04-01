{ pkgs }:

with pkgs; [
  gnutls
  ghostty
  parted
  gparted
  zoxide
  thefuck

  inputs.zen-browser.packages."${system}".default

  vulkan-tools
  vulkan-loader
  vulkan-validation-layers

  unigine-heaven

  ethtool

  emacs

  protontricks
  winetricks
  vulkan-tools
  vulkan-loader
  vulkan-validation-layers
]
