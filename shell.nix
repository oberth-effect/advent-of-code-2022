{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = [
    (pkgs.haskell.packages.ghc902.ghcWithPackages (p:
      with p; [
        megaparsec
        split
        hlint
        haskell-language-server
        cabal-install
        hoogle
        apply-refact
        stylish-haskell
      ]))
  ];
}
