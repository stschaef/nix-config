{ pkgs }:

with pkgs; [
  git
  subversion

  ripgrep
  fd
  gawk
  gnused
  gnutar
  gnugrep
  gnutls
  imagemagick
  zstd

  sqlite
  clang-tools
  age

  fzf
  tree
  wget
  fswatch

  cowsay

  automake
  cmake
  gnumake
  swig

  bash

  binutils
  ffmpeg
  gperf
  libev

  boost
  cppcheck

  vim
  neovim

  # cabal-install
  # ghc
  # haskell-stack
  opam

  coq

  thefuck
  tmux
  zoxide
  zsh
]
