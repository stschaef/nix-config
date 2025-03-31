{ config, pkgs, lib, inputs, system, ... }:

{
  home = {
    username = "stevenschaefer";
    stateVersion = "24.11"; # Please read the comment before changing.
    packages = with pkgs; [
      raycast
    ];
    file = {
    # ".config/doom" = {
    #   source = .dotfiles/doom;
    #   recursive = true;
    # };
    };
    sessionVariables = {
      EDITOR = "emacs";
    };
  };

  programs.git = {
    enable = true;
    userName = "Steven Schaefer";
    userEmail = "stschaef@umich.edu";
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    zplug = {
      enable = true;
      plugins = [
        { name = "mafredri/zsh-async";}
        { name = "sindresorhus/pure"; tags = [ "use:pure.zsh" "as:theme" ];}
        { name = "zdharma/fast-syntax-highlighting"; tags = [ "as:plugin" "defer:2" ];}
        { name = "jeffreytse/zsh-vi-mode"; tags = [ "as:plugin" "defer:2" ];}
      ];
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "thefuck" "zoxide" ];
    };
    initExtra = ''
      ZSH_THEME=""
      source $ZSH/oh-my-zsh.sh
      source <(fzf --zsh)
      eval "$(${pkgs.zoxide}/bin/zoxide init zsh)"
      eval "$(${pkgs.thefuck}/bin/thefuck --alias)"
      function zvm_after_init() {
        zvm_bindkey viins "^R" fzf-history-widget
      }
      zvm_after_init_commands+=('[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh')
    '';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
