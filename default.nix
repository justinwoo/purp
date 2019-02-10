{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "purp";

  src = ./.;

  buildInputs = [
    pkgs.makeWrapper
    pkgs.ghc
  ];

  installPhase = ''
    ghc -o purp purp.hs
    install -D -m555 -t $out/bin purp
    wrapProgram $out/bin/purp \
      --prefix purp_SRC : $src \
      --prefix PATH : $out/bin:${pkgs.lib.makeBinPath [
        pkgs.nix
      ]}

    mkdir -p $out/etc/bash_completion.d/
    cp $src/purp-completion.bash $out/etc/bash_completion.d/purp-completion.bash
  '';
}
