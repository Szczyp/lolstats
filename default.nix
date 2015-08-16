{ aeson, classy-prelude, http-client, lens, lens-aeson, lifted-async, monad-control, mtl,
  pretty-show, text, wreq,

  cabal-install, ghc-mod, stylish-haskell, hoogle, hasktags, hlint,

  mkDerivation, stdenv }:

mkDerivation {
  pname = "lolstats";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ aeson classy-prelude http-client lens lens-aeson lifted-async
                   monad-control mtl pretty-show text wreq ];
  buildTools = [ cabal-install ghc-mod stylish-haskell hoogle hasktags hlint ];
  license = stdenv.lib.licenses.gpl3;
}
