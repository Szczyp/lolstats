with import <nixpkgs> { };

let
  inherit (haskellPackages) cabal cabalInstall_1_20_0_6 ghcMod stylishHaskell hoogle hasktags
  aeson classyPrelude httpClient lens lensAeson liftedAsync monadControl mtl prettyShow text
  wreq;

in cabal.mkDerivation (self: {
  pname = "lolstats";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson classyPrelude httpClient lens lensAeson liftedAsync monadControl mtl prettyShow
    text wreq
  ];
  buildTools = [ cabalInstall_1_20_0_6 ghcMod stylishHaskell hoogle hasktags ];
  meta = {
    description = "lolstats";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
