{ pkgs, inputs, ... }:

let
  acmart-latest = pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
    name = "acmart-latest";
    pname = "acmart";
    version = "2.12";
    passthru = {
      pkgs = [ finalAttrs.finalPackage ];
      tlDeps = with pkgs.texlive; [ latex ];
      tlType = "run";
    };

    srcs = [
      (pkgs.fetchurl {
        url = "https://mirrors.ctan.org/macros/latex/contrib/acmart/acmart.ins";
        sha256 = "sha256-6XFbxY3CahZGQtYzmN3XqBNdh+17xLoQww3kDQK/xRQ=";
      })
      (pkgs.fetchurl {
        url = "https://mirrors.ctan.org/macros/latex/contrib/acmart/acmart.dtx";
        sha256 = "sha256-HOXMA80FeKIC/ggN3JOdVbRtopF/jtF5Yr4CnyR51Hg=";
      })
      (pkgs.fetchurl {
        url = "https://mirrors.ctan.org/macros/latex/contrib/acmart/ACM-Reference-Format.bst";
        sha256 = "sha256-7g2f2Ea5XKi5t3IejZqqBmxkyeemEoTGMVz7uER5Sjk=";
      })
    ];

    unpackPhase = ''
      runHook preUnpack

      for _src in $srcs; do
        cp "$_src" $(stripHash "$_src")
      done

      runHook postUnpack
    '';

    nativeBuildInputs = [ pkgs.texlive.combined.scheme-basic ];

    buildPhase = ''
      runHook preBuild

      # Generate the style files
      latex acmart.ins

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      path="$out/tex/latex/acmart"
      mkdir -p "$path"
      cp *.{cls,def,clo,sty} "$path/"

      # Create directory for BibTeX style files
      bst_path="$out/bibtex/bst/acmart"
      mkdir -p "$bst_path"
      cp ACM-Reference-Format.bst "$bst_path/"

      runHook postInstall
    '';
  });
in
{
  environment.systemPackages = with pkgs; [
    (texlive.combine {
      inherit (pkgs.texlive) scheme-full latexmk;
      pkgFilter = pkg: !(pkg.tlType == "run" &&
                         pkg.pname == "acmart" &&
                         !(pkg ? version && pkg.version == "2.12"));
      acmart = acmart-latest;
    })
  ];
}
