{ config, pkgs, lib, inputs, system, ... }:

{
  home = {
    username = "stevenschaefer";
    stateVersion = "24.11"; # Please read the comment before changing.
    packages = with pkgs; [
      raycast
      aerospace
      lua54Packages.lua
      sketchybar-app-font
      sbar-lua
    ];
    file = {
    ".config/aerospace" = {
      source = ./aerospace;
      recursive = true;
    };
    ".config/sketchybar" = {
      source = ./sketchybar/config;
      recursive = true;
      onChange = "${pkgs.sketchybar}/bin/sketchybar --reload";
    };
    ".local/share/sketchybar_lua/sketchybar.so" = {
      source = "${pkgs.sbar-lua}/lib/sketchybar.so";
      onChange = "${pkgs.sketchybar}/bin/sketchybar --reload";
    };
    ".config/sketchybar/sketchybarrc" = {
      text = ''
        #!/usr/bin/env ${pkgs.lua54Packages.lua}/bin/lua
        package.path = "./?.lua;./?/init.lua;" .. package.path
        -- Load the sketchybar-package and prepare the helper binaries
        require("helpers")
        require("init")
      '';
      executable = true;
      onChange = "${pkgs.sketchybar}/bin/sketchybar --reload";
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
