{ config, pkgs, lib, inputs, system, ... }:

{
  home = {
    username = "steven";
    homeDirectory = "/home/steven";

    stateVersion = "25.05";

    packages = [
    ];

    file = {
      ".agda/libraries".text = ''
        /home/steven/cubical/cubical.agda-lib
        /home/steven/agda-stdlib/standard-library.agda-lib
        /home/steven/cubical-categorical-logic/cubical-categorical-logic.agda-lib
      '';
      ".config/hypr/hyprland.conf".source = ./hyprland/hyprland.conf;
      ".config/waybar/config.jsonc".source = ./hyprland/waybar/config.jsonc;
      ".config/waybar/style.css".source = ./hyprland/waybar/style.css;
    };
  };

  programs.git = {
    enable = true;
    userName = "Steven Schaefer";
    userEmail = "stschaef@umich.edu";
  };
}
