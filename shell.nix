let pkgs = import "/home/qb/projects/nixpkgs" {};
    packageSet = pkgs.haskell.packages.ghc7102;
in (packageSet.callPackage ./. {}).env
