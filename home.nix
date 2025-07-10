{ pkgs, lib, inputs, ... }:

let
  # Placeholder for future variables or configurations
in
{
  home = {
    # Define file locations for configuration files
    file = {
      ".emacs.d" = {
        source = ./emacs/.emacs.d;
        recursive = true;
      };
      ".msmtprc".source = ./emacs/mu4e/.msmtprc;
      ".mbsyncrc".source = ./emacs/mu4e/.mbsyncrc;
    };
    sessionVariables = {
      EDITOR = "emacs";
    };
    packages = with pkgs; [
    ];
  };

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
    initContent = ''
      export PATH=$PATH:~/.cargo/bin
      ZSH_THEME=""
      source $ZSH/oh-my-zsh.sh
      source <(fzf --zsh)
      eval "$(${pkgs.zoxide}/bin/zoxide init zsh)"
      eval "$(${pkgs.thefuck}/bin/thefuck --alias)"
      function zvm_after_init() {
        zvm_bindkey viins "^R" fzf-history-widget
      }
    '';
    shellAliases = {
      cd = "z";
    };
  };

  programs.home-manager.enable = true;
}
