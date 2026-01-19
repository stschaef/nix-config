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

    cabal-install
    (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
       hpkgs.tidal
       hpkgs.fix-whitespace
     ]))

    coq

    cachix

    inputs.forester.legacyPackages.${system}.forester

    ispell

    elan

    nodejs

    uv
    graphviz
  ];
}
