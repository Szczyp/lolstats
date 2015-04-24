{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "lolstats";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ aeson classyPrelude httpClient lens lensAeson
  	       	      		       	 liftedAsync monadControl mtl prettyShow text wreq ];
  buildTools = with haskellPackages; [ cabalInstall_1_20_0_6 ghcMod stylishHaskell hoogle
  	       	    		       hasktags ];
  meta = {
    description = "lolstats";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
