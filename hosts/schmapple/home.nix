{ config, pkgs, lib, inputs, system, ... }:

# let
#   # Create a proper wrapper for Homebrew Emacs
#   homebrewEmacs = pkgs.stdenv.mkDerivation {
#     pname = "emacs-plus-wrapper";
#     version = "1.0.0";
#
#     buildInputs = [];
#
#     # Just create symlinks to the Homebrew binary
#     phases = [ "installPhase" ];
#     installPhase = ''
#       mkdir -p $out/bin
#       ln -s /opt/homebrew/bin/emacs $out/bin/emacs
#       ln -s /opt/homebrew/bin/emacsclient $out/bin/emacsclient
#     '';
#
#     meta = with pkgs.lib; {
#       description = "Wrapper for Homebrew emacs-plus";
#       platforms = platforms.darwin;
#     };
#   };
# in
{
  home = {
    username = "stevenschaefer";
    stateVersion = "24.11"; # Please read the comment before changing.
    packages = with pkgs; [
      raycast
      aerospace
    ];
    file = {
    ".config/aerospace" = {
      source = ./aerospace;
      recursive = true;
    };
    };
    sessionVariables = {
      EDITOR = "emacs";
    };
  };

  xdg = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Steven Schaefer";
    userEmail = "stschaef@umich.edu";
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # imports = [ 
  #   (import ../../emacs.nix {
  #     inherit config pkgs lib;
  #     emacsPackage = homebrewEmacs;
  #   }
  #   )
  # ];

  # programs.emacs-macport = {
  #   enable = true;
  #   # package = homebrewEmacs;
  # };


}
