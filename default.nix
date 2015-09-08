{
  pkgs ? (import <nixpkgs> {}).pkgs,
  haskellPackages ? pkgs.haskellPackages
}:
#let env = pkgs.haskell.packages.ghcHEAD.ghcWithPackages (p: with p; [
let env = haskellPackages.ghcWithPackages (p: with p; [
  string-conv
  http-types
  http-client
  wreq-sb
  aeson
  lens-aeson
  curl
  curl-aeson
  cassava
  bytestring
  cmdargs
  vector
  text
  lens
  tagsoup
  logging
  configurator
  unordered-containers
  safe
  network-uri
  ghc-mod
  cabal-macosx
]); in
pkgs.stdenv.mkDerivation {
  name = "gbm-client";
  src = ./.;
  buildInputs = [env];
  shellHook   = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
