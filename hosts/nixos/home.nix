{ config, pkgs, lib, inputs, system, ... }:

{
  home = {
    username = "steven";
    homeDirectory = "/home/steven";

    stateVersion = "24.11";

    packages = [
    ];

    file = {
    ".agda/libraries".text = ''
      /home/steven/cubical/cubical.agda-lib
      /home/steven/agda-stdlib/standard-library.agda-lib
      /home/steven/cubical-categorical-logic/cubical-categorical-logic.agda-lib
    '';
    };
  };

  programs.git = {
    enable = true;
    userName = "Steven Schaefer";
    userEmail = "stschaef@umich.edu";
  };
}
