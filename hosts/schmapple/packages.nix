{ pkgs }:

with pkgs; [
  hidden-bar
  darwin.sigtool
  darwin.apple_sdk.frameworks.Security
  skimpdf
  pdfpc
]
