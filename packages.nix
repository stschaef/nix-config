{ pkgs, inputs, ... }:

{
  environment.systemPackages = with pkgs; [
    git
    subversion

    bash

    zsh

    zoxide
    thefuck

    tmux
    ripgrep
    fd
    gawk
    gnused
    gnutar
    gnugrep
    gnutls
    fzf
    tree
    wget
    fswatch

    isync
    msmtp

    pass

    automake
    cmake
    gnumake
    swig

    binutils
    ffmpeg
    gperf
    libev

    vim
    neovim

    emacs30

    ghc
    cabal-install
    haskellPackages.Agda

    coq

    cachix

    inputs.forester.legacyPackages.${system}.forester

    ispell

    elan
  ];
}
