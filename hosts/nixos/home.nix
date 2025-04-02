{ config, pkgs, lib, inputs, system, ... }:

{
  xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = {
      modifier = "Mod4";
      gaps = {
        inner = 10;
	outer = 5;
      };
    };
  };

  home = {
    username = "steven";
    homeDirectory = "/home/steven";

    stateVersion = "24.11";

    packages = [
      # inputs.zen-browser.packages."${system}".default
    ];
  };

  programs.git = {
    enable = true;
    userName = "Steven Schaefer";
    userEmail = "stschaef@umich.edu";
  };

  programs.emacs.enable = true;
  programs.emacs.package = pkgs.emacs30;
}
