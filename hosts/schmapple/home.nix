{ config, pkgs, lib, inputs, system, ... }:

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
}
