{ pkgs }:

with pkgs; [
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

  cowsay

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

  ghc
  cabal-install
  haskellPackages.Agda
  # stack
  # haskell-language-server
  # zlib
  # zlib.dev

  # opam
  coq
]
