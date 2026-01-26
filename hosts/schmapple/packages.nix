{ pkgs }:

with pkgs; [
  hidden-bar
  glibtool
  darwin.sigtool
  darwin.apple_sdk.frameworks.Security

  # Emacs 30 with proper daemon support for GUI frames
  ((pkgs.emacsPackagesFor pkgs.emacs30).emacsWithPackages (epkgs: with epkgs; [
    magit
    consult
    hl-todo
    doom-modeline
    vterm
  ]))

  skimpdf
  # pdfpc
]
