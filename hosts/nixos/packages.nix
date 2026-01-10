{ pkgs, inputs, ...}:

with pkgs; [
  gnutls
  ghostty
  parted
  gparted
  zoxide
  thefuck

  vulkan-tools
  vulkan-loader
  vulkan-validation-layers

  unigine-heaven

  ethtool

  emacs30

  protontricks
  winetricks
  vulkan-tools
  vulkan-loader
  vulkan-validation-layers

  inputs.zen-browser.packages."${system}".default


  lutris (
    lutris.override {
      extraPkgs = pkgs: [
        pkgs.libnghttp2
        pkgs.winetricks
      ];
    }
  )
]
