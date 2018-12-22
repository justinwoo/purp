{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "099dacfcdcfa9f239fcb1abbd0a01c3a91d973d5";
    sha256 = "0afds07x4ir0c4cvahgsa6xznz34d2zjrfxbf4vcvrmgv139wrnj";
  });
in pkgs.stdenv.mkDerivation {
  name = "test-purp";
  buildInputs = with easy-ps; [
    purs
    psc-package
  ];
}
