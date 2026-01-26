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
    };
    sessionVariables = {
      EDITOR = "emacsclient -t -a ''";
      VISUAL = "emacsclient -c -a ''";
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

      # Emacs daemon helper function
      emacs-ensure-daemon() {
        if ! emacsclient -e "(+ 1 1)" &>/dev/null; then
          echo "Starting Emacs daemon..."
          emacs --daemon
        fi
      }
    '';
    shellAliases = {
      cd = "z";

      # === Emacs Daemon Commands ===
      # Start daemon (if not running)
      emacs-daemon = "emacs --daemon";

      # Open GUI frame (starts daemon if needed)
      e = "emacsclient -c -n -a ''";
      ec = "emacsclient -c -n -a ''";

      # Open in terminal (starts daemon if needed)
      et = "emacsclient -t -a ''";

      # Open file in existing frame or create new
      ef = "emacsclient -n -a ''";

      # Kill the daemon
      emacs-kill = "emacsclient -e '(kill-emacs)'";

      # Restart daemon
      emacs-restart = "emacsclient -e '(kill-emacs)' 2>/dev/null; sleep 1; emacs --daemon";

      # Check daemon status
      emacs-status = "emacsclient -e '(emacs-pid)' 2>/dev/null && echo 'Daemon running' || echo 'Daemon not running'";
    };
  };

  programs.home-manager.enable = true;
}
