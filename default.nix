with (import <nixpkgs> {});
with haskell.packages.ghc801;
(mkDerivation {
  pname = "lolstats";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ aeson base boxes classy-prelude http-client lens lens-aeson lifted-async
                   monad-control mtl optparse-applicative text tuple wreq ];
  buildTools = [ cabal-install ghc-mod stylish-haskell hoogle hlint ];
  license = stdenv.lib.licenses.gpl3;
}).env
