{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "54266e45aeaebc78dd51a40da36e9840a8a300dd";
    sha256 = "1swjii6975cpys49w5rgnhw5x6ms2cc9fs8ijjpk04pz3zp2vpzn";
  });
  purp = import ../default.nix {};
in pkgs.stdenv.mkDerivation {
  name = "test-purp";
  buildInputs = with easy-ps; [
    purs
    psc-package
    purp
  ];
}
