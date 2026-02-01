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

  ((pkgs.emacsPackagesFor pkgs.emacs30).emacsWithPackages (epkgs: with epkgs; [
    magit
    consult
    hl-todo
    doom-modeline
    vterm
  ]))


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
