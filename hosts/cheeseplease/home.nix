{ config, pkgs, lib, inputs, system, ... }:

{
  home = {
    username = "steven";
    homeDirectory = "/home/steven";

    stateVersion = "25.05";

    packages = [
    ];
  };

  programs.git = {
    enable = true;
    userName = "Steven Schaefer";
    userEmail = "stschaef@umich.edu";
  };
}
