{
  pkgs ? (import <nixpkgs> {}).pkgs,
  haskellPackages ? pkgs.haskellPackages
}:
#let env = pkgs.haskell.packages.ghcHEAD.ghcWithPackages (p: with p; [
let env = haskellPackages.ghcWithPackages (p: with p; [
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
  utf8-string
  logging
  configurator
  unordered-containers
  safe
  network-uri
  ghc-mod
]); in
pkgs.stdenv.mkDerivation {
  name = "crateman";
  src = ./.;
  buildInputs = [env];
  shellHook   = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
