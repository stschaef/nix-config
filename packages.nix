{ pkgs, ... }:


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

    # mu # using homebrew instead because i can't find the mu4e .el's
    isync
    msmtp
    # offlineimap

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
    # stack
    # haskell-language-server
    # zlib
    # zlib.dev

    # opam
    coq

    (texliveFull.withPackages (ps: [
      ps.latexmk
    ]))
  ];
}
