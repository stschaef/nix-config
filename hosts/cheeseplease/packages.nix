{ pkgs, inputs, ...}:

with pkgs; [
  gnutls
  ghostty
  parted
  gparted
  zoxide
  thefuck

  ethtool

  emacs30

  inputs.zen-browser.packages."${system}".default
]
