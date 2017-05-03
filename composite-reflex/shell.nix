let
  reflex-platform = import <reflex-platform> {};

  ghc-platform = import ./platform.nix { inherit reflex-platform; base-platform = reflex-platform.ghc; };
  ghcjs-platform = import ./platform.nix { inherit reflex-platform; };

  ghc-env = reflex-platform.workOn ghc-platform (ghc-platform.callPackage ./package.nix {});
  ghcjs-env = reflex-platform.workOn ghcjs-platform (ghcjs-platform.callPackage ./package.nix {});
in
  ghcjs-env // { ghc = ghc-env; ghcjs-env = ghcjs-env; }
