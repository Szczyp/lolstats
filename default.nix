with (import <nixpkgs> {});
with haskell.packages.ghc7102;
(mkDerivation {
  pname = "lolstats";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ aeson classy-prelude http-client lens lens-aeson lifted-async
                   monad-control mtl optparse-applicative pretty-show text wreq ];
  buildTools = [ cabal-install ghc-mod stylish-haskell hoogle hasktags hlint ];
  license = stdenv.lib.licenses.gpl3;
}).env
