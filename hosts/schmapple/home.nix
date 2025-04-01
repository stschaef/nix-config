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
      (agda.withPackages (ps: [
        ps.standard-library
        # (ps.mkDerivation {
        #   pname = "cubical";
        #   version = "0.7";
        #   src = "/Users/stevenschaefer/cubical/";
        #   libraryFile = "cubical.agda-lib";
        #   libraryName = "cubical";
        #   buildPhase = '''';
        #   dontUnpack = true;
        #   meta = {
        #     description = "Cubical library";
        #     license = lib.licenses.mit;  # Adjust according to the actual license
        #   };
        # })
        # (ps.mkDerivation {
        #   pname = "cubical-categorical-logic";
        #   version = "1.0.0";
        #   src = "/Users/stevenschaefer/cubical-categorical-logic/";
        #   libraryFile = "cubical-categorical-logic.agda-lib";
        #   libraryName = "cubical-categorical-logic";
        #   buildPhase = '''';
        #   dontUnpack = true;
        #   meta = {
        #     description = "Cubical categorical logic library";
        #     license = lib.licenses.mit;  # Adjust according to the actual license
        #   };
        # })
      ]))
    ];
    file = {
    ".config/aerospace" = {
      source = ./aerospace;
      recursive = true;
    };
    ".agda/libraries".text = ''
      /Users/stevenschaefer/cubical/cubical.agda-lib
      /Users/stevenschaefer/agda-stdlib/standard-library.agda-lib
      /Users/stevenschaefer/cubical-categorical-logic/cubical-categorical-logic.agda-lib
    '';
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
